library(dplyr)
library(leaflet)

map_node_locations <- function(node_locs, tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {
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
