
#' Map Calibration Track
#'
#' @param node_locs Node locations dataframe
#' @param sidekick_tag_df Sidekick tag dataframe of a specific tag ID
#' @param tile_url Map URL
#'
#' @returns map
#' @export
#'
#' @examples
#' calibration_map <- map_calibration_track(node_locs,
#'                                          sidekick_tag_df,
#'                                          tile_url = my_tile_url)
map_calibration_track <- function(node_locs,
                                  sidekick_tag_df,
                                  tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {
    map <- leaflet() %>%
        addTiles(
            urlTemplate = tile_url,
            options = tileOptions(maxZoom = 20)
        ) %>%
        addMarkers(
            data = node_locs,
            lat = node_locs$avg_lat,
            lng = node_locs$avg_lon,
            label = node_locs$node_id
        ) %>%
        addPolylines(
            data = sidekick_tag_df,
            lat = sidekick_tag_df$lat,
            lng = sidekick_tag_df$lon,
            weight = 2,
            color = "red"
        ) %>%
        addCircles(
            data = sidekick_tag_df,
            lat = sidekick_tag_df$lat,
            lng = sidekick_tag_df$lon,
            radius = 1.0,
            label = sidekick_tag_df$time_utc,
            stroke = FALSE,
            fillOpacity = 0.5,
            fillColor = "red"
        )
    return(map)
}
