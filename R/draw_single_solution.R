library(leaflet)
library(viridis)

draw_single_solution <- function(rec_df, grid_values, solution, multilat_sol, tile_url) {
    mypal <- colorRampPalette(viridis(1000, option = "D"))(100)

    map2color <- function(x, pal, limits = NULL) {
        if (is.null(limits)) limits <- range(x)
        pal[findInterval(x, seq(limits[1], limits[2], length.out = length(pal) + 1), all.inside = TRUE)]
    }

    rec_with_dets <- subset(rec_df, rec_df$n > 0)

    map <- leaflet() %>%
        addTiles(
            urlTemplate = tile_url,
            options = tileOptions(maxZoom = 20)
        ) %>%
        addRectangles(
            data = grid_values,
            lng1 = grid_values$lon1,
            lat1 = grid_values$lat1,
            lng2 = grid_values$lon2,
            lat2 = grid_values$lat2,
            weight = 0,
            color = map2color(grid_values$value, mypal),
            fillColor = map2color(grid_values$value, mypal),
            fillOpacity = 0.6
            # label = grid_values$value
        ) %>%
        addCircleMarkers(
            data = rec_df,
            lat = rec_df$lat,
            lng = rec_df$lon,
            radius = 5,
            color = "white",
            fillColor = "white",
            fillOpacity = 0.5,
            label = rec_df$node_id,
        ) %>%
        # addCircles(
        #     data = rec_df,
        #     lat = rec_df$lat,
        #     lng = rec_df$lon,
        #     radius = rec_df$exp_dist,
        #     color = "grey",
        #     fillColor = "grey",
        #     fillOpacity = 0.0,
        #     weight = 2
        # ) %>%
        addCircleMarkers(
            data = rec_with_dets,
            lat = rec_with_dets$lat,
            lng = rec_with_dets$lon,
            radius = 5,
            color = "cyan",
            fillColor = "cyan",
            fillOpacity = 0.5,
            label = paste(rec_with_dets$node_id, ", Detections = ", rec_with_dets$n, ", Rssi = ", rec_with_dets$filtered_rssi, sep = "")
        ) %>%
        addRectangles(
            data = solution,
            lng1 = solution$lon1,
            lat1 = solution$lat1,
            lng2 = solution$lon2,
            lat2 = solution$lat2,
            weight = 0,
            color = "red",
            fillColor = "red",
            fillOpacity = 0.5,
            label = paste("Grid Search:",solution$center_lat, ",", solution$center_lon)
        )
        #  %>%
        # Multilat Solution
        # addCircleMarkers(
        #     data = multilat_sol,
        #     lat = multilat_sol[1],
        #     lng = multilat_sol[2],
        #     radius = 2,
        #     color = "orange",
        #     fillColor = "orange",
        #     fillOpacity = 1.0,
        #     label = paste("Multilat:",solution$center_lat, ",", solution$center_lon)
        # )
    return(map)
}
