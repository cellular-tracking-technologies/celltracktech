library(leaflet)
library(viridis)

map_location_density <- function(loc_density_df, tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {
    mypal <- colorRampPalette(viridis(100, option = "D"))(100)

    map2color <- function(x, pal, limits = NULL) {
        if (is.null(limits)) limits <- range(x)
        pal[findInterval(x, seq(limits[1], limits[2], length.out = length(pal) + 1), all.inside = TRUE)]
    }

    map <- leaflet() %>%
        addTiles(
            urlTemplate = tile_url,
            options = tileOptions(maxZoom = 20)
        ) %>%
        addRectangles(
            data = loc_density_df,
            lng1 = loc_density_df$lon1,
            lat1 = loc_density_df$lat1,
            lng2 = loc_density_df$lon2,
            lat2 = loc_density_df$lat2,
            weight = 0,
            color = map2color(loc_density_df$count, mypal),
            fillColor = map2color(loc_density_df$count, mypal),
            fillOpacity = 0.6
            # label = grid_values$value
        ) %>%
        # addCircleMarkers(
        #     data = rec_df,
        #     lat = rec_df$lat,
        #     lng = rec_df$lon,
        #     radius = 5,
        #     color = "white",
        #     fillColor = "white",
        #     fillOpacity = 0.5,
        #     label = rec_df$node_id,
        # ) %>%
        # addCircles(
        #     data = rec_df,
        #     lat = rec_df$lat,
        #     lng = rec_df$lon,
        #     radius = rec_df$exp_dist,
        #     color = "black",
        #     fillColor = "black",
        #     fillOpacity = 0.0,
        #     weight = 2
        # ) %>%
        # addCircleMarkers(
        #     data = rec_with_dets,
        #     lat = rec_with_dets$lat,
        #     lng = rec_with_dets$lon,
        #     radius = 5,
        #     color = "cyan",
        #     fillColor = "cyan",
        #     fillOpacity = 0.5,
        #     label = paste(rec_with_dets$node_id, ", Detections = ", rec_with_dets$n, ", Rssi = ", rec_with_dets$filtered_rssi, sep = "")
        # ) %>%
        # addRectangles(
        #     data = solution,
        #     lng1 = solution$lon1,
        #     lat1 = solution$lat1,
        #     lng2 = solution$lon2,
        #     lat2 = solution$lat2,
        #     weight = 0,
        #     color = "red",
        #     fillColor = "red",
        #     fillOpacity = 0.5,
        #     label = paste(solution$center_lat, ",", solution$center_lon)
        # )
        return(map)
}
