#' Place Nodes Interactively
#'
#' Opens an interactive satellite map where you can click to place node locations
#' and draw a movement path for your study site. Toggle between "Place Nodes" and
#' "Draw Path" modes. Returns node coordinates and path waypoints ready to use
#' with calc_detection_capability() and simulate_tag_movement(). Requires the
#' shiny and miniUI packages.
#'
#' @param center_lat Center latitude of the study site
#' @param center_lon Center longitude of the study site
#' @param zoom Initial map zoom level (default 17, good for ~500m field sites)
#' @param tile_url Map tile URL. Default is Google hybrid satellite imagery.
#' @param rssi_coefs Optional numeric vector of RSSI model coefficients (a, b, c).
#'   If provided, a "Run Analysis" button appears that overlays detection capability
#'   on the map.
#' @param grid_size Grid side length in meters for detection analysis (default 500)
#' @param bin_size Grid cell size in meters for detection analysis (default 10)
#'
#' @returns List with two elements:
#'   \describe{
#'     \item{nodes}{Dataframe with columns: node_id, lat, lon (or NULL if none placed)}
#'     \item{path}{Dataframe with columns: lat, lon of path waypoints (or NULL if
#'       no path drawn)}
#'   }
#'   Returns NULL if cancelled. If analysis was run in-app, the detection
#'   dataframe is attached as attr(result, "detection_df").
#' @export
#'
#' @examples
#' \dontrun{
#' # Place nodes and draw a path
#' result <- place_nodes_interactive(39.0, -76.0)
#' result$nodes  # node locations
#' result$path   # path waypoints
#'
#' # Use with detection pipeline
#' det <- calc_detection_capability(result$nodes, c(-103, -60, 0.012))
#' sim <- simulate_tag_movement(det, result$nodes, c(-103, -60, 0.012),
#'                              waypoints = result$path)
#' map_detection_capability(det, result$nodes, sim_track = sim$track)
#'
#' # With in-app analysis
#' result <- place_nodes_interactive(39.0, -76.0, rssi_coefs = c(-103, -60, 0.012))
#' }
place_nodes_interactive <- function(center_lat,
                                    center_lon,
                                    zoom = 17,
                                    tile_url = "https://mt2.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
                                    rssi_coefs = NULL,
                                    grid_size = 500,
                                    bin_size = 10) {

  if (!interactive()) {
    stop("place_nodes_interactive() requires an interactive R session", call. = FALSE)
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for place_nodes_interactive().\n",
         "Install it with: install.packages('shiny')", call. = FALSE)
  }

  if (!requireNamespace("miniUI", quietly = TRUE)) {
    stop("Package 'miniUI' is required for place_nodes_interactive().\n",
         "Install it with: install.packages('miniUI')", call. = FALSE)
  }

  # Build UI
  analysis_button <- NULL
  if (!is.null(rssi_coefs)) {
    analysis_button <- shiny::actionButton("analyze", "Run Analysis",
                                           icon = shiny::icon("chart-area"))
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Place Nodes & Draw Path"),
    miniUI::miniContentPanel(
      leaflet::leafletOutput("map", width = "100%", height = "100%")
    ),
    miniUI::miniButtonBlock(
      shiny::radioButtons("mode", NULL,
                          choices = c("Place Nodes" = "nodes",
                                      "Draw Path" = "path"),
                          selected = "nodes", inline = TRUE),
      shiny::actionButton("undo", "Undo Last"),
      shiny::actionButton("clear", "Clear All"),
      analysis_button,
      shiny::textOutput("status_text", inline = TRUE)
    )
  )

  # Server logic
  server <- function(input, output, session) {

    # Reactive state
    nodes <- shiny::reactiveVal(
      data.frame(node_id = character(0), lat = numeric(0), lon = numeric(0),
                 stringsAsFactors = FALSE)
    )
    path_pts <- shiny::reactiveVal(
      data.frame(lat = numeric(0), lon = numeric(0),
                 stringsAsFactors = FALSE)
    )
    analysis_result <- shiny::reactiveVal(NULL)

    # Render base map
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles(
          urlTemplate = tile_url,
          options = leaflet::tileOptions(maxZoom = 20)
        ) %>%
        leaflet::setView(lng = center_lon, lat = center_lat, zoom = zoom)
    })

    # Click handler — behavior depends on mode
    shiny::observeEvent(input$map_click, {
      click <- input$map_click

      if (input$mode == "nodes") {
        current <- nodes()
        new_id <- paste0("N", sprintf("%02d", nrow(current) + 1))
        new_node <- data.frame(node_id = new_id, lat = click$lat, lon = click$lng,
                               stringsAsFactors = FALSE)
        nodes(rbind(current, new_node))
        analysis_result(NULL)
      } else {
        current <- path_pts()
        new_pt <- data.frame(lat = click$lat, lon = click$lng,
                             stringsAsFactors = FALSE)
        path_pts(rbind(current, new_pt))
      }
    })

    # Redraw everything when nodes or path change
    shiny::observe({
      current_nodes <- nodes()
      current_path <- path_pts()
      proxy <- leaflet::leafletProxy("map")
      proxy %>% leaflet::clearMarkers() %>% leaflet::clearShapes()

      # Redraw analysis overlay if present
      det <- analysis_result()
      if (!is.null(det)) {
        mypal <- colorRampPalette(viridis::viridis(100, option = "D"))(100)
        values <- det$num_detecting
        map2color <- function(x, pal, limits = NULL) {
          if (is.null(limits)) limits <- range(x, na.rm = TRUE)
          pal[findInterval(x, seq(limits[1], limits[2],
                                  length.out = length(pal) + 1),
                           all.inside = TRUE)]
        }
        proxy %>%
          leaflet::addRectangles(
            data = det,
            lng1 = ~lon1, lat1 = ~lat1, lng2 = ~lon2, lat2 = ~lat2,
            weight = 0,
            color = map2color(values, mypal),
            fillColor = map2color(values, mypal),
            fillOpacity = 0.5,
            label = paste0("Detecting nodes: ", values)
          )
      }

      # Draw node markers
      if (nrow(current_nodes) > 0) {
        proxy %>% leaflet::addCircleMarkers(
          data = current_nodes,
          lat = ~lat, lng = ~lon,
          radius = 8,
          color = "white",
          fillColor = "cyan",
          fillOpacity = 0.9,
          label = ~node_id,
          labelOptions = leaflet::labelOptions(
            noHide = TRUE, direction = "top",
            textOnly = TRUE,
            style = list("color" = "white", "font-weight" = "bold",
                         "text-shadow" = "1px 1px 2px black")
          )
        )
      }

      # Draw path line and waypoint markers
      if (nrow(current_path) > 0) {
        if (nrow(current_path) >= 2) {
          proxy %>% leaflet::addPolylines(
            data = current_path,
            lat = ~lat, lng = ~lon,
            color = "yellow",
            weight = 3,
            opacity = 0.8
          )
        }
        wp_labels <- paste0("WP", seq_len(nrow(current_path)))
        proxy %>% leaflet::addCircleMarkers(
          data = current_path,
          lat = ~lat, lng = ~lon,
          radius = 5,
          color = "yellow",
          fillColor = "yellow",
          fillOpacity = 0.9,
          label = wp_labels,
          labelOptions = leaflet::labelOptions(
            noHide = TRUE, direction = "bottom",
            textOnly = TRUE,
            style = list("color" = "yellow", "font-weight" = "bold",
                         "text-shadow" = "1px 1px 2px black")
          )
        )
      }
    })

    # Undo last — depends on current mode
    shiny::observeEvent(input$undo, {
      if (input$mode == "nodes") {
        current <- nodes()
        if (nrow(current) > 0) {
          current <- current[-nrow(current), , drop = FALSE]
          if (nrow(current) > 0) {
            current$node_id <- paste0("N", sprintf("%02d", seq_len(nrow(current))))
          }
          nodes(current)
        }
        analysis_result(NULL)
      } else {
        current <- path_pts()
        if (nrow(current) > 0) {
          path_pts(current[-nrow(current), , drop = FALSE])
        }
      }
    })

    # Clear all — clears both nodes and path
    shiny::observeEvent(input$clear, {
      nodes(data.frame(node_id = character(0), lat = numeric(0), lon = numeric(0),
                       stringsAsFactors = FALSE))
      path_pts(data.frame(lat = numeric(0), lon = numeric(0),
                          stringsAsFactors = FALSE))
      analysis_result(NULL)
    })

    # Status display
    output$status_text <- shiny::renderText({
      paste0("Nodes: ", nrow(nodes()), " | Path: ", nrow(path_pts()), " waypoints")
    })

    # Run detection analysis
    if (!is.null(rssi_coefs)) {
      shiny::observeEvent(input$analyze, {
        current <- nodes()
        if (nrow(current) < 1) {
          shiny::showNotification("Place at least one node first.", type = "warning")
          return()
        }

        shiny::withProgress(message = "Calculating detection capability...", {
          det <- calc_detection_capability(
            node_locs = current,
            rssi_coefs = rssi_coefs,
            grid_size = grid_size,
            bin_size = bin_size
          )
        })

        analysis_result(det)

        det_prob <- attr(det, "detection_prob")
        loc_prob <- attr(det, "location_prob")
        shiny::showNotification(
          sprintf("Detection: %.1f%% | Location: %.1f%%",
                  det_prob * 100, loc_prob * 100),
          type = "message",
          duration = 10
        )
      })
    }

    # Done — return nodes and path
    shiny::observeEvent(input$done, {
      result_nodes <- nodes()
      result_path <- path_pts()

      if (nrow(result_nodes) == 0 && nrow(result_path) == 0) {
        message("No nodes or path placed.")
        shiny::stopApp(NULL)
      } else {
        result <- list(
          nodes = if (nrow(result_nodes) > 0) result_nodes else NULL,
          path = if (nrow(result_path) > 0) result_path else NULL
        )
        det <- analysis_result()
        if (!is.null(det)) {
          attr(result, "detection_df") <- det
        }
        shiny::stopApp(result)
      }
    })

    # Cancel
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  # Launch gadget in RStudio viewer
  viewer <- shiny::paneViewer(minHeight = 500)
  shiny::runGadget(ui, server, viewer = viewer)
}
