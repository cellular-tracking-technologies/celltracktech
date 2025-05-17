
#' Map Node Locations
#'
#' @param node_locs - data frame
#' @param tile_url - url to map
#'
#' @returns - map
#' @export
#'
#' @examples
#' node_map <- map_node_locations(node_locs, tile_url = my_tile_url)

map_node_locations <- function(node_locs,
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
        )

    return(map)
}
