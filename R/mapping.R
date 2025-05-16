#' Mapping
#'
#' @param node_locs
#' @param track_error_df
#' @param tile_url
#'
#' @returns map
#' @export
#'
#' @examples
mapping <- function(node_locs,
                    track_error_df,
                    tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {
  map <- leaflet() %>%
    addTiles(
      urlTemplate = tile_url,
      options = tileOptions(maxZoom = 25)
    ) %>%
    addCircleMarkers(
      data = node_locs,
      lat = node_locs$node_lat,
      lng = node_locs$node_lng,
      radius = 5,
      color = "cyan",
      fillColor = "cyan",
      fillOpacity = 0.5,
      label = node_locs$node_id
    )  %>%
    #addPolylines(
    # data = track_error_df,
    #lat = track_error_df$lat_est,
    # lng = track_error_df$lon_est,
    #color = "red",
    #  weight = 2
    #) %>%
    addCircleMarkers(
      data = track_error_df,
      lat = track_error_df$lat_est,
      lng = track_error_df$lon_est,
      radius = 1,
      color = "red",
      fillColor = "red",
      fillOpacity = 1.0)
  #label = paste(track_error_df$i, ":", as_datetime(track_error_df$time), " : ", track_error_df$error)
  return(map)
}
