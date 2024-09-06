require(httr2)
require(jsonlite)

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
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' get_sensors_data(nc, c("name", "pm2.5", "humidity"))
get_sensor_data <- function(sensor_index, fields = NULL, api_key = NULL, geometry = FALSE,...){
  if (suppressWarnings(!is.na(as.numeric(sensor_index)))){
    if (is.null(api_key)){
      api_key = Sys.getenv("PURPLEAIR_API_KEY")
    }
    if (is.null(fields)){
      if (geometry) {
        fields = c("latitude", "longitude")
      }
      qry <- list(sensor_index = sensor_index,
                  api_key = api_key)
    }
    else {
      if (geometry) {
        fields = append(fields, values = c("latitude", "longitude"))
      }
      qry <- list(sensor_index = sensor_index,
                  fields = fields,
                  api_key = api_key)
    }

    resp_df <- get_purple_req(qry, scope = "sensor", sensor_id = sensor_index)

  if (geometry == FALSE){
    return(resp_df)
  }
  else {
    resp_sf <- resp_to_sf(resp_df, 4269)
    return(resp_sf)
  }
  }
  else {
    stop("'sensor_index' must be numeric")
  }
}
