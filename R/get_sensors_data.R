require(httr2)
require(jsonlite)
require(sf)

#' Retrieves a list with the latest data of multiple sensors matching the provided parameters.
#'
#' @param aoi Optional; an sf object defining an area of interest
#' @param fields A character vector of fields specified in the [PurpleAir API documentation](https://api.purpleair.com/#api-sensors-get-sensors-data)
#' @param api_key If not set with purpleair_api_key()
#' @param ...
#'
#' @return A POINT sf object
#' @export
#'
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' get_sensors_data(nc, c("name", "pm2.5", "humidity"))
get_sensors_data <- function(aoi = NULL, fields, api_key = Sys.getenv("PURPLEAIR_API_KEY"), ...){
  if (is.null(aoi)){
    qry <- list(fields = append(fields, values = c("latitude", "longitude")),
                api_key = api_key)
  }
  else if (!inherits(aoi, "sf")){
    stop("'aoi' must be an sf object.")
  }
  else {
    aoi_bb <- st_bbox(aoi, crs = 4269)
    qry <- list(fields = append(fields, values = c("latitude", "longitude")),
                api_key = api_key,
                nwlat = aoi_bb["ymax"],
                nwlng = aoi_bb["xmin"],
                selat = aoi_bb["ymin"],
                selng = aoi_bb["xmax"])
  }

  error_body <- function(resp) {
    resp_body_json(resp)$description
  }

  req <- request("https://api.purpleair.com/v1/sensors") |>
    req_url_query(!!!qry, .multi = "comma") |>
    req_error(body = error_body)

  resp <- req_perform(req) |>
    resp_body_raw() |>
    rawToChar() |>
    parse_json(simplifyVector = T)

  resp_df <- as.data.frame(resp[["data"]])
  names(resp_df) <- resp[["fields"]]

  resp_sf <- st_as_sf(resp_df, coords = c("longitude", "latitude"), crs = 4269) |>
    st_transform(st_crs(aoi))

  return(resp_sf[aoi,])
}
