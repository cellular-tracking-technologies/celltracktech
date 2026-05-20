#' Plot Detection Capability
#'
#' Creates a ggplot2 heatmap showing predicted detection capability for a custom
#' node layout. Optionally overlays a simulated tag track.
#'
#' @param detection_df Dataframe returned by calc_detection_capability()
#' @param node_locs Dataframe of node positions with columns: node_id, lat, lon
#'   (also accepts avg_lat/avg_lon)
#' @param value_col Column name to fill by: "num_detecting" (default),
#'   "best_rssi", "mean_rssi", "can_detect", or "can_locate"
#' @param sim_track Optional track dataframe from simulate_tag_movement()$track.
#'   If provided, the simulated path is overlaid colored by detection strength
#'   (best_rssi) and error circles shown at locatable positions.
#' @param theme Optional ggplot2 theme to apply
#'
#' @returns ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' nodes <- generate_node_layout(8, "circle", 39.0, -76.0, 150)
#' rssi_coefs <- c(-103, -60, 0.012)
#' det <- calc_detection_capability(nodes, rssi_coefs)
#' plot_detection_capability(det, nodes)
#'
#' # With simulated track
#' sim <- simulate_tag_movement(det, nodes, rssi_coefs)
#' plot_detection_capability(det, nodes, sim_track = sim$track)
#' }
plot_detection_capability <- function(detection_df,
                                      node_locs,
                                      value_col = "num_detecting",
                                      sim_track = NULL,
                                      theme = NULL) {

  # Accept output from place_nodes_interactive() directly
  if (is.list(node_locs) && "nodes" %in% names(node_locs)) {
    node_locs <- node_locs$nodes
  }

  # Normalize node location column names
  if ("avg_lat" %in% names(node_locs) && !"lat" %in% names(node_locs)) {
    node_locs$lat <- node_locs$avg_lat
    node_locs$lon <- node_locs$avg_lon
  }

  fill_label <- switch(value_col,
    num_detecting = "Detecting\nNodes",
    best_rssi = "Best RSSI\n(dBm)",
    mean_rssi = "Mean RSSI\n(dBm)",
    can_detect = "Can\nDetect",
    can_locate = "Can\nLocate",
    value_col
  )

  plot <- ggplot() +
    geom_rect(data = detection_df,
              aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2,
                  fill = .data[[value_col]])) +
    scale_fill_viridis_c(option = "D", name = fill_label) +
    geom_point(data = node_locs,
               aes(x = lon, y = lat),
               shape = 21, fill = "white", color = "black", size = 3) +
    geom_text(data = node_locs,
              aes(x = lon, y = lat, label = node_id),
              vjust = -1.2, size = 3) +
    labs(title = "Detection Capability",
         x = "Longitude", y = "Latitude") +
    coord_fixed()

  # Overlay simulated track if provided
  if (!is.null(sim_track)) {
    plot <- plot +
      geom_path(data = sim_track,
                aes(x = lon, y = lat),
                color = "black", linewidth = 0.3, alpha = 0.3) +
      geom_point(data = sim_track,
                 aes(x = lon, y = lat, color = best_rssi),
                 size = 1, alpha = 0.7) +
      scale_color_viridis_c(option = "D", name = "RSSI\n(dBm)")

    # Show error circles at locatable positions (sampled to avoid clutter)
    locatable <- sim_track[!is.na(sim_track$location_error_m), ]
    if (nrow(locatable) > 0) {
      sampled <- locatable[seq(1, nrow(locatable), by = 10), ]
      plot <- plot +
        ggplot2::geom_point(data = sampled,
                            aes(x = lon, y = lat,
                                size = location_error_m),
                            shape = 1, color = "orange", alpha = 0.5) +
        ggplot2::scale_size_continuous(name = "Error\n(m)", range = c(1, 8))
    }
  }

  if (!is.null(theme)) {
    plot <- plot + theme
  }

  return(plot)
}
