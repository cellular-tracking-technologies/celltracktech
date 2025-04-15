library(ggplot2)

plot_node_locations <- function(node_health_df, 
                                node_locs,
                                theme = NULL) {
    plot <- ggplot() +
        geom_point(
            data = node_locs,
            aes(
                x = avg_lon,
                y = avg_lat,
                colour = node_id
            ),
            shape = 15,
            size = 3
        ) +
        geom_point(
            data = node_health_df,
            aes(
                x = longitude,
                y = latitude,
                colour = node_id
            ),
            shape = 1,
            size = 1
        ) +
        xlab("Longitude") +
        ylab("Latitude")

    return(plot + theme)
}
