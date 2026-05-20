#' Calculate Detection Capability
#'
#' Predicts tag detection strength across a spatial area for a custom node layout.
#' For each grid cell, computes the predicted RSSI from every node and aggregates
#' detection metrics including strongest signal, number of detecting nodes, mean
#' RSSI of detecting nodes, and whether detection and localization are possible.
#'
#' @param node_locs Dataframe of node positions with columns: node_id, lat, lon
#'   (also accepts avg_lat/avg_lon)
#' @param rssi_coefs Numeric vector of RSSI model coefficients (a, b, c) where
#'   RSSI = a - b * exp(-c * distance)
#' @param grid_df Optional custom grid dataframe with columns: i, lat1, lon1, lat2,
#'   lon2, center_lat, center_lon. If provided, grid_size, bin_size, center_lat,
#'   and center_lon are ignored.
#' @param grid_size Grid side length in meters (default 500). Ignored if grid_df
#'   is provided.
#' @param bin_size Grid cell size in meters (default 10). Ignored if grid_df is
#'   provided.
#' @param center_lat Center latitude of analysis area. Defaults to mean of node
#'   latitudes. Ignored if grid_df is provided.
#' @param center_lon Center longitude of analysis area. Defaults to mean of node
#'   longitudes. Ignored if grid_df is provided.
#' @param min_nodes_locate Minimum number of detecting nodes required for
#'   localization (default 3, for trilateration)
#'
#' @returns Dataframe with grid cell geometry and detection metrics:
#'   best_rssi, num_detecting, mean_rssi, nearest_node_dist, can_detect,
#'   can_locate. Also prints overall detection_prob and location_prob as
#'   attributes on the returned dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' nodes <- generate_node_layout(8, "circle", 39.0, -76.0, 150)
#' rssi_coefs <- c(-103, -60, 0.012)
#' detection <- calc_detection_capability(nodes, rssi_coefs)
#' attr(detection, "detection_prob")   # fraction of area with >= 1 node
#' attr(detection, "location_prob")    # fraction of area with >= 3 nodes
#'
#' # With a custom grid
#' my_grid <- build_grid(nodes, 39.0, -76.0, 300, 300, 5)
#' detection <- calc_detection_capability(nodes, rssi_coefs, grid_df = my_grid)
#' }
calc_detection_capability <- function(node_locs,
                                      rssi_coefs,
                                      grid_df = NULL,
                                      grid_size = 500,
                                      bin_size = 10,
                                      center_lat = NULL,
                                      center_lon = NULL,
                                      min_nodes_locate = 3) {


  # Accept output from place_nodes_interactive() directly
  if (is.list(node_locs) && "nodes" %in% names(node_locs)) {
    node_locs <- node_locs$nodes
  }

  # Normalize node location column names
  if ("avg_lat" %in% names(node_locs) && !"lat" %in% names(node_locs)) {
    node_locs$lat <- node_locs$avg_lat
    node_locs$lon <- node_locs$avg_lon
  }

  # Build grid if not provided
  if (is.null(grid_df)) {
    if (is.null(center_lat)) center_lat <- mean(node_locs$lat)
    if (is.null(center_lon)) center_lon <- mean(node_locs$lon)
    grid_df <- build_grid(node_locs, center_lat, center_lon,
                          grid_size, grid_size, bin_size)
  }

  # Create all cell-node pairs
  grid_cells <- grid_df[, c("i", "center_lat", "center_lon")]
  node_points <- node_locs[, c("node_id", "lat", "lon")]

  pairs <- tidyr::crossing(grid_cells, node_points)

  # Vectorized distance and RSSI calculations
  pairs$dist <- haversine(pairs$lat, pairs$lon,
                          pairs$center_lat, pairs$center_lon)
  pairs$pred_rssi <- predict_rssi(rssi_coefs, pairs$dist)
  pairs$detected <- pairs$pred_rssi > rssi_coefs[1]

  # Aggregate per grid cell
  result <- dplyr::group_by(pairs, i)
  result <- dplyr::summarise(result,
    best_rssi = max(pred_rssi),
    num_detecting = sum(detected),
    mean_rssi = ifelse(any(detected), mean(pred_rssi[detected]), NA_real_),
    nearest_node_dist = min(dist),
    .groups = "drop"
  )

  grid_df <- dplyr::left_join(grid_df, result, by = "i")

  # Detection and location flags
  grid_df$can_detect <- grid_df$num_detecting >= 1
  grid_df$can_locate <- grid_df$num_detecting >= min_nodes_locate

  # Overall probabilities (fraction of grid cells)
  n_cells <- nrow(grid_df)
  detection_prob <- sum(grid_df$can_detect) / n_cells
  location_prob <- sum(grid_df$can_locate) / n_cells

  attr(grid_df, "detection_prob") <- detection_prob
  attr(grid_df, "location_prob") <- location_prob

  message(sprintf("Detection probability: %.1f%% of area (>= 1 node detects)",
                  detection_prob * 100))
  message(sprintf("Location probability:  %.1f%% of area (>= %d nodes detect)",
                  location_prob * 100, min_nodes_locate))

  return(grid_df)
}
