api_key_pattern <- "^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$"

api_key_checker <- function(x){
  if (inherits(x, "character")
      & length(x) == 1
      & grepl(x = x,
              pattern = api_key_pattern)){
    TRUE
  }
  else if (!grepl(x = x,
                  pattern = api_key_pattern)){
    warning("The provided 'api_key' does not match the format provided by PurpleAir.")
    TRUE
  }
  else {
    stop("'api_key' must be a character vector.")
    FALSE
  }
}

require(httr2)
require(jsonlite)

get_purple_req <- function(qry, scope, sensor_id = NULL){
  error_body <- function(resp) {
    resp_body_json(resp)$description
  }

  if (scope == "sensors") {
    url_suffix <- ""
  }
  else if (scope == "sensor"){
    url_suffix <- sensor_id
  }
  else if (scope == "history"){
    url_suffix <- paste0(sensor_id, "/history")
  }

  req <- httr2::request(paste0("https://api.purpleair.com/v1/sensors/", url_suffix)) |>
    httr2::req_url_query(!!!qry, .multi = "comma") |>
    httr2::req_error(body = error_body)

  resp <- req_perform(req) |>
    resp_body_raw() |>
    rawToChar() |>
    parse_json(simplifyVector = T)

  if (scope == "sensors"){
    resp_df <- as.data.frame(resp[["data"]])
    names(resp_df) <- resp[["fields"]]
  }

  if (scope == "sensor"){
    resp_df <- as.data.frame(resp)
    resp_names <- names(resp_df)[startsWith(names(resp_df), "sensor")]
    resp_idx <- match(resp_names, names(resp_df))
    resp_df <- resp_df[,resp_idx]
    resp_names <- names(resp_df) |>
      gsub(pattern = "sensor.", replacement = "", fixed = T)
    names(resp_df) <- resp_names
  }

  if (scope == "history") {
    resp_df <- as.data.frame(resp[["data"]])
    names(resp_df) <- resp[["fields"]]
    resp_df$time_stamp <- as.POSIXct(resp_df$time_stamp, format = "%Y-%m-%dT%TZ")
    resp_df <- resp_df[order(resp_df$time_stamp),]
  }

  return(resp_df)
}

resp_to_sf <- function(resp_df, crs){
  resp_df$latitude <- as.numeric(resp_df$latitude)
  resp_df$longitude <- as.numeric(resp_df$longitude)

  resp_sf <- st_as_sf(resp_df[!is.na(resp_df$latitude)|!is.na(resp_df$longitude),],
                      coords = c("longitude", "latitude"),
                      crs = 4269) |>
    st_transform(crs)

  if (length(resp_df[is.na(resp_df$latitude)|is.na(resp_df$longitude),] > 0)){
    resp_sf <- resp_sf |>
      rbind({ng <- resp_df[is.na(resp_df$latitude)|is.na(resp_df$longitude),]
      ng$geometry = st_sfc(st_point(),
                           crs = 4269)
      ng <- st_as_sf(ng) |>
        st_transform(crs)
      ng <- subset(ng,
                   select = -c(latitude, longitude)) |>
        st_as_sf()
      ng}) |>
      st_as_sf()
  }
  return(resp_sf)
}
