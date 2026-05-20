#' Simulate Tag Movement
#'
#' Simulates a tag moving along a user-defined path through a node layout.
#' Computes detection strength (RSSI), number of detecting nodes, and
#' localization error at each interpolated step along the path.
#'
#' @param detection_df Dataframe returned by calc_detection_capability()
#' @param node_locs Dataframe of node positions with columns: node_id, lat, lon
#'   (also accepts avg_lat/avg_lon)
#' @param rssi_coefs Numeric vector of RSSI model coefficients (a, b, c)
#' @param waypoints Dataframe of path waypoints with columns: lat, lon. The tag
#'   walks between consecutive waypoints in order. If NULL, the tag visits all
#'   nodes in random order.
#' @param step_size_m Distance between interpolated steps in meters (default 5)
#' @param min_nodes_locate Minimum number of detecting nodes required for
#'   localization (default 3)
#'
#' @returns List with:
#'   \describe{
#'     \item{track}{Dataframe with columns: step, lat, lon, num_detecting,
#'       best_rssi, mean_rssi, location_error_m (estimated localization error
#'       in meters)}
#'     \item{detection_rate}{Fraction of steps where tag was detected (>= 1 node)}
#'     \item{location_rate}{Fraction of steps where tag was locatable (>= min_nodes_locate nodes)}
#'     \item{mean_error_m}{Mean localization error in meters (for locatable steps)}
#'     \item{summary}{Summary dataframe with statistics by num_detecting}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' nodes <- generate_node_layout(8, "circle", 39.0, -76.0, 150)
#' rssi_coefs <- c(-103, -60, 0.012)
#' det <- calc_detection_capability(nodes, rssi_coefs)
#'
#' # User-defined path
#' path <- data.frame(lat = c(39.0005, 39.001, 38.999, 39.0),
#'                    lon = c(-76.001, -75.999, -76.0, -76.001))
#' sim <- simulate_tag_movement(det, nodes, rssi_coefs, waypoints = path)
#'
#' # Auto path visiting all nodes
#' sim <- simulate_tag_movement(det, nodes, rssi_coefs)
#' }
simulate_tag_movement <- function(detection_df,
                                  node_locs,
                                  rssi_coefs,
                                  waypoints = NULL,
                                  step_size_m = 5,
                                  min_nodes_locate = 3) {

  # Accept output from place_nodes_interactive() directly
  if (is.list(node_locs) && "nodes" %in% names(node_locs)) {
    if (is.null(waypoints) && !is.null(node_locs$path)) {
      waypoints <- node_locs$path
    }
    node_locs <- node_locs$nodes
  }

  # Normalize node location column names
  if ("avg_lat" %in% names(node_locs) && !"lat" %in% names(node_locs)) {
    node_locs$lat <- node_locs$avg_lat
    node_locs$lon <- node_locs$avg_lon
  }

  # Coordinate conversion constants
  center_lat <- mean(c(min(detection_df$lat1), max(detection_df$lat2)))
  meters_per_deg_lat <- 111320
  meters_per_deg_lon <- 111320 * cos(center_lat * pi / 180)

  # Build waypoints if not provided: visit all nodes in random order
  if (is.null(waypoints)) {
    visit_order <- sample(seq_len(nrow(node_locs)))
    waypoints <- data.frame(
      lat = c(center_lat, node_locs$lat[visit_order]),
      lon = c(mean(c(min(detection_df$lon1), max(detection_df$lon2))),
              node_locs$lon[visit_order])
    )
  }

  if (nrow(waypoints) < 2) {
    stop("waypoints must have at least 2 rows", call. = FALSE)
  }

  # Interpolate between waypoints at step_size_m spacing
  track_lat <- numeric(0)
  track_lon <- numeric(0)

  for (w in 2:nrow(waypoints)) {
    from_lat <- waypoints$lat[w - 1]
    from_lon <- waypoints$lon[w - 1]
    to_lat <- waypoints$lat[w]
    to_lon <- waypoints$lon[w]

    dlat <- to_lat - from_lat
    dlon <- to_lon - from_lon
    dist_m <- sqrt((dlat * meters_per_deg_lat)^2 + (dlon * meters_per_deg_lon)^2)
    n_interp <- max(1, round(dist_m / step_size_m))

    seg_lats <- seq(from_lat, to_lat, length.out = n_interp + 1)
    seg_lons <- seq(from_lon, to_lon, length.out = n_interp + 1)

    # Add all points except the last (start of next segment), except for final segment
    if (w == nrow(waypoints)) {
      track_lat <- c(track_lat, seg_lats)
      track_lon <- c(track_lon, seg_lons)
    } else {
      track_lat <- c(track_lat, seg_lats[-(n_interp + 1)])
      track_lon <- c(track_lon, seg_lons[-(n_interp + 1)])
    }
  }

  actual_steps <- length(track_lat)

  # Calculate detection metrics and localization error at each position
  num_detecting <- numeric(actual_steps)
  best_rssi <- numeric(actual_steps)
  mean_rssi <- numeric(actual_steps)
  location_error_m <- numeric(actual_steps)

  for (t in seq_len(actual_steps)) {
    dists <- haversine(node_locs$lat, node_locs$lon,
                       track_lat[t], track_lon[t])
    rssi_vals <- predict_rssi(rssi_coefs, dists)
    detected <- rssi_vals > rssi_coefs[1]
    n_det <- sum(detected)

    num_detecting[t] <- n_det
    best_rssi[t] <- max(rssi_vals)
    mean_rssi[t] <- if (n_det > 0) mean(rssi_vals[detected]) else NA_real_

    # Estimate localization error using grid search
    if (n_det >= min_nodes_locate) {
      location_error_m[t] <- .estimate_location_error(
        true_lat = track_lat[t], true_lon = track_lon[t],
        node_lats = node_locs$lat[detected],
        node_lons = node_locs$lon[detected],
        node_rssi = rssi_vals[detected],
        rssi_coefs = rssi_coefs,
        detection_df = detection_df
      )
    } else {
      location_error_m[t] <- NA_real_
    }
  }

  track_df <- data.frame(
    step = seq_len(actual_steps),
    lat = track_lat,
    lon = track_lon,
    num_detecting = num_detecting,
    best_rssi = best_rssi,
    mean_rssi = mean_rssi,
    location_error_m = location_error_m,
    stringsAsFactors = FALSE
  )

  detection_rate <- mean(num_detecting >= 1)
  location_rate <- mean(num_detecting >= min_nodes_locate)
  locatable_errors <- location_error_m[!is.na(location_error_m)]
  mean_error <- if (length(locatable_errors) > 0) mean(locatable_errors) else NA_real_

  # Summary by number of detecting nodes
  summary_df <- as.data.frame(table(num_detecting = track_df$num_detecting))
  summary_df$fraction <- summary_df$Freq / actual_steps

  message(sprintf("Simulation complete: %d steps along %d waypoints",
                  actual_steps, nrow(waypoints)))
  message(sprintf("Detection rate: %.1f%% (>= 1 node)", detection_rate * 100))
  message(sprintf("Location rate:  %.1f%% (>= %d nodes)", location_rate * 100,
                  min_nodes_locate))
  if (!is.na(mean_error)) {
    message(sprintf("Mean localization error: %.1f m", mean_error))
  }

  list(
    track = track_df,
    detection_rate = detection_rate,
    location_rate = location_rate,
    mean_error_m = mean_error,
    summary = summary_df
  )
}

# Estimate localization error at a single point by finding the grid cell with
# highest likelihood (inverse RSSI residual) and computing distance to true position
.estimate_location_error <- function(true_lat, true_lon,
                                     node_lats, node_lons, node_rssi,
                                     rssi_coefs, detection_df) {
  # For each grid cell, compute sum of squared RSSI differences
  grid_lats <- detection_df$center_lat
  grid_lons <- detection_df$center_lon

  total_diff2 <- rep(0, nrow(detection_df))

  for (n in seq_along(node_lats)) {
    dists <- haversine(node_lats[n], node_lons[n], grid_lats, grid_lons)
    expected_rssi <- predict_rssi(rssi_coefs, dists)
    diff2 <- (node_rssi[n] - expected_rssi)^2
    total_diff2 <- total_diff2 + diff2
  }

  # Best estimate = grid cell with smallest total squared difference
  best_idx <- which.min(total_diff2)
  est_lat <- grid_lats[best_idx]
  est_lon <- grid_lons[best_idx]

  # Error = distance between true and estimated position
  haversine(true_lat, true_lon, est_lat, est_lon)
}
