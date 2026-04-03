#' Plot Node Locations
#'
#' @param node_health_df Node health dataframe
#' @param node_locs Node locations
#' @param theme ggplot2 theme
#'
#' @returns plot
#' @export
#'
#' @examples
#' node_loc_plot <- plot_node_locations(node_health_df,
#'                                      node_locs,
#'                                      theme = classic_plot_theme())

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
