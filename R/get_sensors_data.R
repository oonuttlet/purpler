require(httr2)
require(jsonlite)
require(sf)

#' Retrieves a list with the latest data of multiple sensors matching the provided parameters.
#'
#' @param aoi Optional; an sf object defining an area of interest
#' @param fields A character vector of fields specified in the [PurpleAir API documentation](https://api.purpleair.com/#api-sensors-get-sensors-data)
#' @param api_key If not previously set with purpleair_api_key()
#' @param ...
#'
#' @return A POINT sf object
#' @export
#'
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' get_sensors_data(nc, c("name", "pm2.5", "humidity"))
get_sensors_data <- function(aoi = NULL,
                             fields,
                             api_key = NULL,
                             ...){ #TODO: other api options
  if (is.null(api_key)){
    api_key = Sys.getenv("PURPLEAIR_API_KEY")
  }
  if (is.null(aoi)){
    qry <- list(fields = append(fields,
                                values = c("latitude", "longitude")),
                api_key = api_key)
    crs <- 4269
  }
  else if (!inherits(aoi, "sf")){
    stop("'aoi' must be an sf object.")
  }
  else {
    crs <- st_crs(aoi)
    aoi_bb <- st_bbox(aoi, crs = 4269)
    qry <- list(fields = append(fields, values = c("latitude", "longitude")),
                api_key = api_key,
                nwlat = aoi_bb["ymax"],
                nwlng = aoi_bb["xmin"],
                selat = aoi_bb["ymin"],
                selng = aoi_bb["xmax"])
  }

  resp_df <- get_purple_req(qry, "sensors")

  resp_sf <- resp_to_sf(resp_df, crs = crs)

  if (!is.null(aoi)){
    resp_sf <- resp_sf[aoi,]
  }

  return(resp_sf)
}
