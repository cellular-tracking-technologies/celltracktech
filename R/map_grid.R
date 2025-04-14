library(leaflet)

draw_grid <- function(node_locs, grid_df) {
    map <- leaflet() %>%
        addTiles(
            options = tileOptions(maxZoom = 20)
        ) %>%
        addMarkers(
            data = node_locs,
            lat = node_locs$avg_lat,
            lng = node_locs$avg_lon,
            label = node_locs$node_id
        ) %>%
        addRectangles(
            data = grid_df,
            lng1 = grid_df$lon1,
            lat1 = grid_df$lat1,
            lng2 = grid_df$lon2,
            lat2 = grid_df$lat2,
            weight = 1,
            color = "black",
            fillOpacity = 0.0
        )
    return(map)
}
