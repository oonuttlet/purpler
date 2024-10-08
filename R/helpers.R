api_key_pattern <- "^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$"

check_pattern_api_key <- function(x) {
  if (inherits(x, "character") &
    length(x) == 1 &
    grepl(
      x = x,
      pattern = api_key_pattern
    )) {
    cat(cli::combine_ansi_styles("br_green", "bold")("Success!\n"))
    TRUE
  } else if (!grepl(
    x = x,
    pattern = api_key_pattern
  )) {
    cat(cli::combine_ansi_styles("br_yellow", "bold")("Warning: The provided 'api_key' does not match the format provided by PurpleAir.\n"))
    TRUE
  } else {
    stop("'api_key' must be a character vector.")
    FALSE
  }
}

# Helper function to check and set environment variables

check_renv_api_key <- function(api_key, renv, overwrite) {
  oldenv <- read.table(renv, stringsAsFactors = FALSE)
  oldenv$V1 <- oldenv[, 1] |> strsplit("=", fixed = TRUE)
  oldenv$V2 <- sapply(oldenv$V1, FUN = function(x) x[2])
  oldenv$V1 <- sapply(oldenv$V1, FUN = function(x) x[1])

  # Check if the API key already exists and handle overwrite
  if (isTRUE(overwrite)) {
    if (length(grep("PURPLEAIR_API_KEY", oldenv$V1)) > 0) {
      message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
      oldenv <- oldenv[-grep("PURPLEAIR_API_KEY", oldenv$V1), ]
    }
  } else if (any(grepl("PURPLEAIR_API_KEY", readLines(renv)))) {
    stop("A PURPLEAIR_API_KEY already exists. You can overwrite it with the argument overwrite=TRUE.", call. = FALSE)
  }

  # Add the new API key
  tr <- data.frame(V1 = "PURPLEAIR_API_KEY", V2 = paste0("'", api_key, "'"))
  newenv <- rbind(tr, oldenv)

  # Write the new .Renviron
  write.table(newenv, renv, sep = "=", quote = FALSE, col.names = FALSE, row.names = FALSE)
  message("Your API key has been stored in your .Renviron.")
}

requireNamespace("httr2", quietly = T)
requireNamespace("jsonlite", quietly = T)

get_purple_req <- function(qry, scope, sensor_id = NULL) {
  error_body <- function(resp) {
    resp_body_json(resp)$description
  }

  if (scope == "sensors") {
    url_suffix <- ""
  } else if (scope == "sensor") {
    url_suffix <- sensor_id
  } else if (scope == "history") {
    url_suffix <- paste0(sensor_id, "/history")
  }

  req <- httr2::request(paste0("https://api.purpleair.com/v1/sensors/", url_suffix)) |>
    httr2::req_url_query(!!!qry, .multi = "comma") |>
    httr2::req_error(body = error_body)

  resp <- req_perform(req) |>
    resp_body_raw() |>
    rawToChar() |>
    parse_json(simplifyVector = T)

  if (scope == "sensors") {
    resp_df <- as.data.frame(resp[["data"]])
    names(resp_df) <- resp[["fields"]]
  }

  if (scope == "sensor") {
    resp_df <- as.data.frame(resp)
    resp_names <- names(resp_df)[startsWith(names(resp_df), "sensor")]
    resp_idx <- match(resp_names, names(resp_df))
    resp_df <- resp_df[, resp_idx]
    resp_names <- names(resp_df) |>
      gsub(pattern = "sensor.", replacement = "", fixed = T)
    names(resp_df) <- resp_names
  }

  if (scope == "history") {
    resp_df <- as.data.frame(resp[["data"]])
    names(resp_df) <- resp[["fields"]]
    resp_df$time_stamp <- as.POSIXct(resp_df$time_stamp, format = "%Y-%m-%dT%TZ")
    resp_df <- resp_df[order(resp_df$time_stamp), ]
  }

  return(resp_df)
}

resp_to_sf <- function(resp_df, crs) {
  resp_df$latitude <- as.numeric(resp_df$latitude)
  resp_df$longitude <- as.numeric(resp_df$longitude)

  resp_sf <- st_as_sf(resp_df[!is.na(resp_df$latitude) | !is.na(resp_df$longitude), ],
    coords = c("longitude", "latitude"),
    crs = 4269
  ) |>
    st_transform(crs)

  if (length(resp_df[is.na(resp_df$latitude) | is.na(resp_df$longitude), ] > 0)) {
    resp_sf <- resp_sf |>
      rbind({
        ng <- resp_df[is.na(resp_df$latitude) | is.na(resp_df$longitude), ]
        ng$geometry <- st_sfc(st_point(),
          crs = 4269
        )
        ng <- st_as_sf(ng) |>
          st_transform(crs)
        ng <- subset(ng,
          select = -c(latitude, longitude)
        ) |>
          st_as_sf()
        ng
      }) |>
      st_as_sf()
  }
  return(resp_sf)
}
