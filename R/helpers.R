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

get_sensors_req <- function(qry){
  error_body <- function(resp) {
    resp_body_json(resp)$description
  }

  req <- httr2::request("https://api.purpleair.com/v1/sensors") |>
    httr2::req_url_query(!!!qry, .multi = "comma") |>
    httr2::req_error(body = error_body)

  resp <- req_perform(req) |>
    resp_body_raw() |>
    rawToChar() |>
    parse_json(simplifyVector = T)

  resp_df <- as.data.frame(resp[["data"]])
  names(resp_df) <- resp[["fields"]]

  return(resp_df)
}
