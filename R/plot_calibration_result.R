library(ggplot2)

plot_calibration_result <- function(rssi_v_dist, theme = NULL) {
    plot <- ggplot() +
        geom_point(
            data = rssi_v_dist,
            aes(
                x = distance,
                y = rssi,
                colour = node_id
            )
        ) +
        geom_line(
            data = rssi_v_dist,
            aes(
                x = distance,
                y = pred
            ),
            linewidth = 1.0
        ) +
        labs(title = "RSSI vs. Distance", x = "Distance (m)", y = "RSSI (dBm)", colour = "Node ID")

    return(plot + theme)
}
