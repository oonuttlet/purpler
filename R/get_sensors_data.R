require(httr2)
require(jsonlite)
require(sf)

get_sensors_data <- function(aoi = NULL, fields, api_key = Sys.getenv("PURPLEAIR_API_KEY"), ...){
  if (is.null(aoi)){
    qry <- list(fields = append(fields, values = c("latitude", "longitude")),
                api_key = api_key)
  }
  else if (!inherits(aoi, sf)){
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
