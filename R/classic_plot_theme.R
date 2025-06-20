#' Classic Plot Theme
#'
#' @returns creates classic theme for ggplots
#' @export
#'
#' @examples
#' node_loc_plot <- plot_node_locations(node_health_df,
#'                                      node_locs,
#'                                      theme = classic_plot_theme())

classic_plot_theme <- function() {
  theme(
    axis.text.x = element_text(size = 20, vjust = 0.5),
    axis.text.y = element_text(size = 20, vjust = 0.5),
    axis.title.x = element_text(size = 20, vjust = 0.5),
    axis.title.y = element_text(size = 20, vjust = 0.5),
    axis.line = element_line(colour = "black", linewidth = 0.5),
    panel.grid.major = element_line(colour = "grey", linewidth = 0.25),
    panel.grid.minor = element_line(colour = "grey", linewidth = 0.1),
    panel.background = element_rect(fill = "white"),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "inches"),
    plot.title = element_text(
        size = 25,
        face = "bold",
        hjust = 0.5
    ),
  )
}
