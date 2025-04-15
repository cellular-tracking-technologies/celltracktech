calculate_node_locations <- function(node_health_df) {
    node_ids <- sort(unique(node_health_df$node_id))

    result <- data.frame(
        node_id = character(),
        n_lat = integer(),
        avg_lat = double(),
        sd_lat = double(),
        n_lon = integer(),
        avg_lon = double(),
        sd_lon = double(),
        stringsAsFactors = TRUE
    )

    for (i in seq_along(node_ids)) {
        lat_values <- subset(node_health_df, node_id == node_ids[i])$latitude
        lon_values <- subset(node_health_df, node_id == node_ids[i])$longitude

        lat_values <- lat_values[!is.na(lat_values)]
        lon_values <- lon_values[!is.na(lon_values)]

        n_lat <- length(lat_values)
        n_lon <- length(lon_values)

        if (n_lat != n_lon) {
            print("Error: different number lat & lon values!!!")
        } else if (n_lat == 0) {
            print("No valid locations for this node")
        } else {
            avg_lat <- mean(lat_values)
            avg_lon <- mean(lon_values)

            sd_lat <- sd(lat_values)
            sd_lon <- sd(lon_values)

            if (n_lat == 1) {
                sd_lat <- 0
                sd_lon <- 0
            }
            this_result <- data.frame(
                node_id = node_ids[i],
                n_lat = length(lat_values),
                avg_lat = avg_lat,
                sd_lat = sd_lat,
                n_lon = length(lon_values),
                avg_lon = avg_lon,
                sd_lon = sd_lon,
                stringsAsFactors = TRUE
            )
            result <- rbind(result, this_result)
        }
    }

    return(result)
}
