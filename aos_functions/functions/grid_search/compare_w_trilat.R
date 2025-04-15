library(dplyr)
library(leaflet)
library(lubridate)

map_multilat <- function(
    node_locs,
    track_error_df,
    sidekick_df,
    multilat_df,
    tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {
    error_lines <- data.frame(
        id = integer(),
        lat = double(),
        lon = double()
    )
    for (i in 1:nrow(track_error_df)) {
        track_solution <- track_error_df[i, ]
        line_start <- data.frame(
            id = track_solution$i,
            lat = track_solution$act_lat,
            lon = track_solution$act_lon
        )
        line_end <- data.frame(
            id = track_solution$i,
            lat = track_solution$sol_lat,
            lon = track_solution$sol_lon
        )
        error_lines <- rbind(error_lines, line_start)
        error_lines <- rbind(error_lines, line_end)
    }

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
            data = sidekick_df,
            lat = sidekick_df$lat,
            lng = sidekick_df$lon,
            color = "blue",
            weight = 2
        ) %>%
        addCircleMarkers(
            data = sidekick_df,
            lat = sidekick_df$lat,
            lng = sidekick_df$lon,
            radius = 1,
            color = "blue",
            fillColor = "blue",
            fillOpacity = 1.0,
            label = as_datetime(sidekick_df$time_value)
        ) %>%
        addPolylines(
            data = track_error_df,
            lat = track_error_df$sol_lat,
            lng = track_error_df$sol_lon,
            color = "red",
            weight = 2
        ) %>%
        addCircleMarkers(
            data = track_error_df,
            lat = track_error_df$sol_lat,
            lng = track_error_df$sol_lon,
            radius = 1,
            color = "red",
            fillColor = "red",
            fillOpacity = 1.0,
            label = paste(track_error_df$i, ":", as_datetime(track_error_df$time), " : ", track_error_df$error)
        ) %>%
        addPolylines(
            data = multilat_df,
            lat = multilat_df$lat_est,
            lng = multilat_df$lon_est,
            color = "orange",
            weight = 2
        ) %>%
        addCircleMarkers(
            data = multilat_df,
            lat = multilat_df$lat_est,
            lng = multilat_df$lon_est,
            radius = 1,
            color = "orange",
            fillColor = "orange",
            fillOpacity = 1.0,
            # label = paste(track_error_df$i, ":", as_datetime(track_error_df$time), " : ", track_error_df$error)
        )
    return(map)
}
