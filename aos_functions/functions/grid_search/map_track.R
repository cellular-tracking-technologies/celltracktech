library(dplyr)
library(leaflet)
library(lubridate)

map_track <- function(node_locs, track_df, tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {
    map <- leaflet() %>%
        addTiles(
            urlTemplate = tile_url,
            options = tileOptions(maxZoom = 25)
        ) %>%
        addCircleMarkers(
            data = node_locs,
            lat = node_locs$avg_lat,
            lng = node_locs$avg_lon,
            radius = 5,
            color = "cyan",
            fillColor = "cyan",
            fillOpacity = 0.5,
            label = node_locs$node_id
        ) %>%
        addPolylines(
            data = track_df,
            lat = track_df$lat,
            lng = track_df$lon,
            color = "red",
            weight = 2
        ) %>%
        addCircleMarkers(
            data = track_df,
            lat = track_df$lat,
            lng = track_df$lon,
            radius = 1,
            color = "red",
            fillColor = "red",
            fillOpacity = 1.0,
            label = as_datetime(track_df$time)
        )
        #  %>%
        # Multilat Solutions
        # addPolylines(
        #     data = track_df,
        #     lat = track_df$ml_lat,
        #     lng = track_df$ml_lon,
        #     color = "orange",
        #     weight = 2
        # ) %>%
        # addCircleMarkers(
        #     data = track_df,
        #     lat = track_df$ml_lat,
        #     lng = track_df$ml_lon,
        #     radius = 1,
        #     color = "orange",
        #     fillColor = "orange",
        #     fillOpacity = 1.0,
        #     label = as_datetime(track_df$time)
        # )
    return(map)
}
