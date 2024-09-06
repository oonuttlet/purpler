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
get_sensors_data <- function(aoi = NULL, fields, api_key = NULL, ...){
  if (is.null(api_key)){
    api_key = Sys.getenv("PURPLEAIR_API_KEY")
  }
  if (is.null(aoi)){
    qry <- list(fields = append(fields, values = c("latitude", "longitude")),
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

  resp_df <- get_sensors_req(qry)

  resp_df$latitude <- as.numeric(resp_df$latitude)
  resp_df$longitude <- as.numeric(resp_df$longitude)

  resp_sf <- st_as_sf(resp_df[!is.na(resp_df$latitude)|!is.na(resp_df$longitude),], coords = c("longitude", "latitude"), crs = 4269) |>
    st_transform(crs)
  if (length(resp_df[is.na(resp_df$latitude)|is.na(resp_df$longitude),] > 0)){
    resp_sf <- resp_sf |>
      rbind({ng <- resp_df[is.na(resp_df$latitude)|is.na(resp_df$longitude),]
             ng$geometry = st_sfc(st_point(), crs = 4269)
             ng <- st_as_sf(ng) |>
                st_transform(crs)
             ng <- subset(ng, select = -c(latitude, longitude)) |>
                st_as_sf()
             ng}) |>
      st_as_sf()
  }

  if (!is.null(aoi)){
    resp_sf <- resp_sf[aoi,]
  }

  return(resp_sf)
}


