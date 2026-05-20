#' Map Detection Capability
#'
#' Creates a leaflet map showing node positions with an optional simulated tag
#' track overlay colored by RSSI detection strength and localization error circles.
#'
#' @param detection_df Dataframe returned by calc_detection_capability()
#' @param node_locs Dataframe of node positions with columns: node_id, lat, lon
#'   (also accepts avg_lat/avg_lon)
#' @param sim_track Optional track dataframe from simulate_tag_movement()$track.
#'   If provided, the simulated path is overlaid on the map.
#' @param max_error_m Maximum location error in meters to display. Error circles
#'   larger than this value are hidden. Default NULL shows all.
#' @param tile_url Base map tile URL
#'
#' @returns leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' nodes <- generate_node_layout(8, "circle", 39.0, -76.0, 150)
#' rssi_coefs <- c(-103, -60, 0.012)
#' det <- calc_detection_capability(nodes, rssi_coefs)
#' sim <- simulate_tag_movement(det, nodes, rssi_coefs)
#' map_detection_capability(det, nodes, sim_track = sim$track)
#' map_detection_capability(det, nodes, sim_track = sim$track, max_error_m = 50)
#' }
map_detection_capability <- function(detection_df,
                                     node_locs,
                                     sim_track = NULL,
                                     max_error_m = NULL,
                                     tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {

  # Accept output from place_nodes_interactive() directly
  if (is.list(node_locs) && "nodes" %in% names(node_locs)) {
    node_locs <- node_locs$nodes
  }

  # Normalize node location column names
  if ("avg_lat" %in% names(node_locs) && !"lat" %in% names(node_locs)) {
    node_locs$lat <- node_locs$avg_lat
    node_locs$lon <- node_locs$avg_lon
  }

  map <- leaflet() %>%
    addTiles(
      urlTemplate = tile_url,
      options = tileOptions(maxZoom = 20)
    ) %>%
    addCircleMarkers(
      data = node_locs,
      lat = node_locs$lat,
      lng = node_locs$lon,
      radius = 6,
      color = "white",
      fillColor = "black",
      fillOpacity = 0.8,
      label = node_locs$node_id
    )

  if (!is.null(sim_track)) {
    track_pal <- viridis::viridis(100, option = "D")

    map2color <- function(x, pal, limits = NULL) {
      if (is.null(limits)) limits <- range(x, na.rm = TRUE)
      pal[findInterval(x, seq(limits[1], limits[2], length.out = length(pal) + 1),
                       all.inside = TRUE)]
    }

    rssi_vals <- sim_track$best_rssi
    rssi_colors <- map2color(rssi_vals, track_pal)

    track_labels <- paste0(
      "Step ", sim_track$step,
      " | RSSI: ", round(sim_track$best_rssi, 1), " dBm",
      " | Nodes: ", sim_track$num_detecting,
      ifelse(!is.na(sim_track$location_error_m),
             paste0(" | Error: ", round(sim_track$location_error_m, 1), " m"),
             " | No location fix")
    )

    map <- map %>%
      addPolylines(
        data = sim_track,
        lat = sim_track$lat,
        lng = sim_track$lon,
        color = "black",
        weight = 1,
        opacity = 0.3
      ) %>%
      addCircleMarkers(
        data = sim_track,
        lat = sim_track$lat,
        lng = sim_track$lon,
        radius = 3,
        color = rssi_colors,
        fillColor = rssi_colors,
        fillOpacity = 0.8,
        stroke = FALSE,
        label = track_labels
      )

    # Error circles
    locatable <- sim_track[!is.na(sim_track$location_error_m), ]
    if (!is.null(max_error_m)) {
      locatable <- locatable[locatable$location_error_m <= max_error_m, ]
    }
    if (nrow(locatable) > 0) {
      sampled <- locatable[seq(1, nrow(locatable), by = 10), ]
      if (nrow(sampled) > 0) {
        map <- map %>%
          addCircles(
            data = sampled,
            lat = sampled$lat,
            lng = sampled$lon,
            radius = sampled$location_error_m,
            color = "orange",
            fillColor = "orange",
            fillOpacity = 0.1,
            weight = 1,
            label = paste0("Error: ", round(sampled$location_error_m, 1), " m")
          )
      }
    }

    # RSSI legend
    map <- map %>%
      addLegend(
        position = "topleft",
        colors = rev(track_pal[seq(1, 100, length.out = 5)]),
        labels = round(rev(seq(min(rssi_vals, na.rm = TRUE),
                               max(rssi_vals, na.rm = TRUE),
                               length.out = 5)), 1),
        title = "Track RSSI (dBm)"
      )

    # Error circle legend
    if (nrow(locatable) > 0) {
      error_vals <- locatable$location_error_m
      error_breaks <- round(seq(min(error_vals), max(error_vals), length.out = 4), 1)
      circle_sizes <- round(seq(8, 30, length.out = 4))
      circle_html <- paste0(
        '<div style="display:flex;align-items:center;margin:3px 0;">',
        '<svg width="', circle_sizes * 2, '" height="', circle_sizes * 2,
        '" style="margin-right:8px;">',
        '<circle cx="', circle_sizes, '" cy="', circle_sizes,
        '" r="', circle_sizes - 1,
        '" fill="orange" fill-opacity="0.2" stroke="orange" stroke-width="1"/>',
        '</svg>',
        '<span>', error_breaks, ' m</span></div>'
      )
      legend_html <- paste0(
        '<div style="padding:6px 8px;background:white;border-radius:5px;',
        'box-shadow:0 0 15px rgba(0,0,0,0.2);">',
        '<div style="font-weight:bold;margin-bottom:5px;">Location Error</div>',
        paste(circle_html, collapse = ""),
        '</div>'
      )
      map <- map %>%
        addControl(html = legend_html, position = "bottomright")
    }
  }

  return(map)
}
