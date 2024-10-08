#' Retrieves current sensor information for a single sensor (defined by 'sensor_index').
#'
#' @param sensor_index A single numeric value corresponding to a specific PurpleAir sensor
#' @param fields Optional; a character vector of fields specified in the [PurpleAir API documentation](https://api.purpleair.com/#api-sensors-get-sensors-data). If left blank, will default to all fields specified in the documentation
#' @param api_key If not set with purpleair_api_key()
#' @param geometry Optional (default FALSE); if TRUE, an sf object will be returned.
#' @param ...
#'
#' @return A POINT sf object
#' @export
#'
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#' get_sensors_data(nc, c("name", "pm2.5", "humidity"))
get_sensor_data <- function(sensor_index,
                            fields = NULL,
                            start_date = NULL,
                            end_date = NULL,
                            average = NULL,
                            geometry = FALSE,
                            api_key = NULL,
                            ...) {
  stopifnot(
    "'sensor_index' must be numeric" = suppressWarnings(!is.na(as.numeric(sensor_index))),
    "'average' must be numeric" = suppressWarnings(!is.na(as.numeric(average))),
    "'start_date' and 'end_date' must be valid ISO8601 strings (e.g., '2024-01-01')" =
      grepl(pattern = r"(^\d{4}-\d{2}-\d{2}$)", x = c(start_date, end_date))
  )

  scope <- "history"

  if (is.null(api_key)) {
    api_key <- Sys.getenv("PURPLEAIR_API_KEY")
  }

  if (is.null(start_date) & is.null(end_date) & is.null(average)) {
    scope <- "sensor"
  }

  if (is.null(fields) & scope == "history") {
    stop("For historical data, 'fields' cannot be null")
  }

  if (geometry & scope == "history") {
    stop("For historical data, 'geometry' cannot be TRUE")
  }

  if (geometry & scope == "sensor") {
    fields <- c(fields, "latitude", "longitude")
  }

  qry <- list(
    sensor_index = sensor_index,
    fields = fields,
    api_key = api_key
  )

  if (scope == "history") {
    qry <- append(qry, list(
      start_timestamp = format(as.POSIXct(start_date, tz = "UTC"), format = "%Y-%m-%dT%TZ", tz = "UTC"),
      end_timestamp = format(as.POSIXct(end_date, tz = "UTC"), format = "%Y-%m-%dT%TZ", tz = "UTC"),
      average = average
    ))
  }

  resp_df <- get_purple_req(qry, scope = scope, sensor_id = sensor_index)

  if (geometry == FALSE) {
    return(resp_df)
  } else {
    resp_sf <- resp_to_sf(resp_df, 4269)
    return(resp_sf)
  }
}
