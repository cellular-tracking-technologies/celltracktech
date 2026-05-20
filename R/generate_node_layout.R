#' Generate Node Layout
#'
#' Creates a dataframe of node positions arranged in a specified shape. By default,
#' nodes are evenly distributed along the perimeter. When n_rows and n_cols are
#' provided, nodes are placed in a grid pattern and clipped to the shape boundary,
#' filling the interior with rows and columns.
#'
#' @param n_nodes Number of nodes to place along the perimeter. Ignored when both
#'   n_rows and n_cols are provided.
#' @param shape Shape of the layout: "circle", "square", "rectangle", "oval",
#'   "star", "hexagon", "triangle", "line", or "grid"
#' @param center_lat Center latitude of the layout
#' @param center_lon Center longitude of the layout
#' @param size_m Radius (for circle/oval/star/hexagon) or half-side-length
#'   (for square/rectangle/triangle/grid) or half-length (for line) in meters
#' @param width_m Width in meters for rectangle, oval, and grid shapes. For
#'   rectangle/grid this is the half-width (size_m is half-height). For oval,
#'   this is the semi-minor axis (size_m is semi-major). Ignored for other shapes.
#' @param n_rows Number of rows for interior grid fill. When provided with n_cols,
#'   nodes are arranged in rows and columns clipped to the shape boundary.
#' @param n_cols Number of columns for interior grid fill. When provided with
#'   n_rows, nodes are arranged in rows and columns clipped to the shape boundary.
#' @param n_points Number of star points (default 5). Only used when shape is
#'   "star".
#' @param inner_ratio Ratio of inner to outer radius for star shape (default 0.4).
#'   Only used when shape is "star".
#'
#' @returns Dataframe with columns: node_id, lat, lon
#' @export
#'
#' @examples
#' \dontrun{
#' # 8 nodes around a circle perimeter
#' nodes <- generate_node_layout(8, "circle", 39.0, -76.0, 100)
#'
#' # 4x4 grid filling a circle
#' nodes <- generate_node_layout(shape = "circle", center_lat = 39.0,
#'                               center_lon = -76.0, size_m = 100,
#'                               n_rows = 4, n_cols = 4)
#'
#' # 5x3 grid filling a rectangle
#' nodes <- generate_node_layout(shape = "rectangle", center_lat = 39.0,
#'                               center_lon = -76.0, size_m = 100,
#'                               width_m = 60, n_rows = 5, n_cols = 3)
#'
#' # 6x6 grid filling a star shape
#' nodes <- generate_node_layout(shape = "star", center_lat = 39.0,
#'                               center_lon = -76.0, size_m = 150,
#'                               n_rows = 6, n_cols = 6)
#'
#' # Simple 4x5 rectangular grid
#' nodes <- generate_node_layout(shape = "grid", center_lat = 39.0,
#'                               center_lon = -76.0, size_m = 100,
#'                               width_m = 80, n_rows = 4, n_cols = 5)
#' }
generate_node_layout <- function(n_nodes = NULL,
                                 shape = "circle",
                                 center_lat,
                                 center_lon,
                                 size_m,
                                 width_m = NULL,
                                 n_rows = NULL,
                                 n_cols = NULL,
                                 n_points = 5,
                                 inner_ratio = 0.4) {

  # Convert meters to approximate lat/lon offsets
  meters_per_deg_lat <- 111320
  meters_per_deg_lon <- 111320 * cos(center_lat * pi / 180)

  # If n_rows and n_cols provided, fill the shape interior with a grid
  if (!is.null(n_rows) && !is.null(n_cols)) {
    offsets <- .layout_filled_shape(shape, size_m, width_m, n_rows, n_cols,
                                   n_points, inner_ratio)
  } else {
    if (is.null(n_nodes) || n_nodes < 1) stop("n_nodes must be at least 1")

    offsets <- switch(shape,
      circle = .layout_circle(n_nodes, size_m),
      square = .layout_polygon(n_nodes, size_m, 4),
      rectangle = .layout_rectangle(n_nodes, size_m, width_m %||% size_m),
      oval = .layout_oval(n_nodes, size_m, width_m %||% (size_m * 0.6)),
      star = .layout_star(n_nodes, size_m, n_points, inner_ratio),
      hexagon = .layout_polygon(n_nodes, size_m, 6),
      triangle = .layout_polygon(n_nodes, size_m, 3),
      line = .layout_line(n_nodes, size_m),
      grid = .layout_grid(n_nodes, size_m, width_m %||% size_m),
      stop(paste0("Unknown shape: '", shape, "'. Use one of: circle, square, ",
                  "rectangle, oval, star, hexagon, triangle, line, grid"))
    )
  }

  n_total <- length(offsets$x)
  data.frame(
    node_id = paste0("N", sprintf("%02d", seq_len(n_total))),
    lat = center_lat + offsets$y / meters_per_deg_lat,
    lon = center_lon + offsets$x / meters_per_deg_lon,
    stringsAsFactors = FALSE
  )
}

# --- Interior grid fill for any shape ---

.layout_filled_shape <- function(shape, size_m, width_m, n_rows, n_cols,
                                 n_points, inner_ratio) {
  # Determine bounding box of the shape
  half_h <- size_m
  half_w <- switch(shape,
    circle = size_m,
    square = size_m,
    rectangle = width_m %||% size_m,
    oval = width_m %||% (size_m * 0.6),
    star = size_m,
    hexagon = size_m,
    triangle = size_m,
    grid = width_m %||% size_m,
    line = size_m,
    size_m
  )

  # For grid/square/rectangle/line, span the full bounding box
  if (shape %in% c("grid", "square", "rectangle", "line")) {
    xs <- seq(-half_w, half_w, length.out = n_cols)
    ys <- seq(-half_h, half_h, length.out = n_rows)
    grid_pts <- expand.grid(x = xs, y = ys)
    return(list(x = grid_pts$x, y = grid_pts$y))
  }

  # For curved/irregular shapes, oversample the grid (2x density) then clip,
  # so interior coverage is good even after boundary clipping
  oversample <- 2
  xs <- seq(-half_w, half_w, length.out = n_cols * oversample)
  ys <- seq(-half_h, half_h, length.out = n_rows * oversample)
  grid_pts <- expand.grid(x = xs, y = ys)

  inside <- .point_in_shape(grid_pts$x, grid_pts$y, shape, size_m, width_m,
                            n_points, inner_ratio)
  kept <- grid_pts[inside, ]

  # Thin to approximately n_rows * n_cols nodes using k-means clustering
  target_n <- n_rows * n_cols
  if (nrow(kept) > target_n) {
    km <- stats::kmeans(as.matrix(kept), centers = target_n, nstart = 3)
    kept <- as.data.frame(km$centers)
    names(kept) <- c("x", "y")
  }

  message(sprintf("  %s shape with %d rows x %d cols: %d nodes placed",
                  shape, n_rows, n_cols, nrow(kept)))
  list(x = kept$x, y = kept$y)
}

# Test whether points fall inside a shape boundary
.point_in_shape <- function(px, py, shape, size_m, width_m,
                            n_points, inner_ratio) {
  switch(shape,
    circle = {
      (px^2 + py^2) <= size_m^2
    },
    oval = {
      semi_minor <- width_m %||% (size_m * 0.6)
      (px / size_m)^2 + (py / semi_minor)^2 <= 1
    },
    hexagon = {
      # Get hexagon vertices and use point-in-polygon
      angles <- seq(pi / 2, pi / 2 + 2 * pi, length.out = 7)[1:6]
      vx <- size_m * cos(angles)
      vy <- size_m * sin(angles)
      .points_in_polygon(px, py, vx, vy)
    },
    triangle = {
      angles <- seq(pi / 2, pi / 2 + 2 * pi, length.out = 4)[1:3]
      vx <- size_m * cos(angles)
      vy <- size_m * sin(angles)
      .points_in_polygon(px, py, vx, vy)
    },
    star = {
      inner_radius <- size_m * inner_ratio
      n_vertices <- n_points * 2
      vertex_angles <- seq(pi / 2, pi / 2 + 2 * pi,
                           length.out = n_vertices + 1)[1:n_vertices]
      vx <- numeric(n_vertices)
      vy <- numeric(n_vertices)
      for (v in seq_len(n_vertices)) {
        r <- if (v %% 2 == 1) size_m else inner_radius
        vx[v] <- r * cos(vertex_angles[v])
        vy[v] <- r * sin(vertex_angles[v])
      }
      .points_in_polygon(px, py, vx, vy)
    },
    # Default: keep all points
    rep(TRUE, length(px))
  )
}

# Ray casting point-in-polygon test
.points_in_polygon <- function(px, py, vx, vy) {
  n_verts <- length(vx)
  inside <- logical(length(px))

  for (p in seq_along(px)) {
    x <- px[p]
    y <- py[p]
    count <- 0
    j <- n_verts
    for (i in seq_len(n_verts)) {
      if (((vy[i] > y) != (vy[j] > y)) &&
          (x < (vx[j] - vx[i]) * (y - vy[i]) / (vy[j] - vy[i]) + vx[i])) {
        count <- count + 1
      }
      j <- i
    }
    inside[p] <- (count %% 2) == 1
  }
  inside
}

# --- Perimeter layout functions ---

# Points evenly spaced around a circle
.layout_circle <- function(n, radius) {
  angles <- seq(0, 2 * pi, length.out = n + 1)[1:n]
  list(x = radius * cos(angles), y = radius * sin(angles))
}

# Points evenly spaced along a regular polygon perimeter
.layout_polygon <- function(n, radius, n_sides) {
  vertex_angles <- seq(pi / 2, pi / 2 + 2 * pi, length.out = n_sides + 1)[1:n_sides]
  vx <- radius * cos(vertex_angles)
  vy <- radius * sin(vertex_angles)

  seg_lengths <- numeric(n_sides)
  for (s in seq_len(n_sides)) {
    next_s <- (s %% n_sides) + 1
    seg_lengths[s] <- sqrt((vx[next_s] - vx[s])^2 + (vy[next_s] - vy[s])^2)
  }
  total_perim <- sum(seg_lengths)

  .distribute_along_segments(n, vx, vy, seg_lengths, total_perim, n_sides)
}

# Points evenly spaced along a rectangle perimeter
.layout_rectangle <- function(n, half_height, half_width) {
  vx <- c(-half_width, half_width, half_width, -half_width)
  vy <- c(half_height, half_height, -half_height, -half_height)
  n_sides <- 4

  seg_lengths <- numeric(n_sides)
  for (s in seq_len(n_sides)) {
    next_s <- (s %% n_sides) + 1
    seg_lengths[s] <- sqrt((vx[next_s] - vx[s])^2 + (vy[next_s] - vy[s])^2)
  }
  total_perim <- sum(seg_lengths)

  .distribute_along_segments(n, vx, vy, seg_lengths, total_perim, n_sides)
}

# Points evenly spaced around an ellipse
.layout_oval <- function(n, semi_major, semi_minor) {
  angles <- seq(0, 2 * pi, length.out = n + 1)[1:n]
  list(x = semi_major * cos(angles), y = semi_minor * sin(angles))
}

# Points distributed around a star shape
.layout_star <- function(n, outer_radius, n_points, inner_ratio) {
  inner_radius <- outer_radius * inner_ratio
  n_vertices <- n_points * 2

  vertex_angles <- seq(pi / 2, pi / 2 + 2 * pi,
                       length.out = n_vertices + 1)[1:n_vertices]
  vx <- numeric(n_vertices)
  vy <- numeric(n_vertices)
  for (v in seq_len(n_vertices)) {
    r <- if (v %% 2 == 1) outer_radius else inner_radius
    vx[v] <- r * cos(vertex_angles[v])
    vy[v] <- r * sin(vertex_angles[v])
  }

  seg_lengths <- numeric(n_vertices)
  for (s in seq_len(n_vertices)) {
    next_s <- (s %% n_vertices) + 1
    seg_lengths[s] <- sqrt((vx[next_s] - vx[s])^2 + (vy[next_s] - vy[s])^2)
  }
  total_perim <- sum(seg_lengths)

  .distribute_along_segments(n, vx, vy, seg_lengths, total_perim, n_vertices)
}

# Points evenly spaced along a line
.layout_line <- function(n, half_length) {
  if (n == 1) return(list(x = 0, y = 0))
  x <- seq(-half_length, half_length, length.out = n)
  list(x = x, y = rep(0, n))
}

# Nodes in a filled rectangular grid
.layout_grid <- function(n, half_height, half_width) {
  # Find best rows x cols that fits n nodes
  n_cols <- round(sqrt(n * half_width / half_height))
  n_cols <- max(1, n_cols)
  n_rows <- ceiling(n / n_cols)

  xs <- seq(-half_width, half_width, length.out = n_cols)
  ys <- seq(-half_height, half_height, length.out = n_rows)
  grid_pts <- expand.grid(x = xs, y = ys)
  grid_pts <- grid_pts[seq_len(min(n, nrow(grid_pts))), ]
  list(x = grid_pts$x, y = grid_pts$y)
}

# --- Shared helpers ---

# Distribute n points evenly along connected line segments
.distribute_along_segments <- function(n, vx, vy, seg_lengths, total_perim,
                                       n_segments) {
  spacing <- total_perim / n
  x <- numeric(n)
  y <- numeric(n)
  cum_dist <- 0
  node_idx <- 1

  for (s in seq_len(n_segments)) {
    next_s <- (s %% n_segments) + 1
    seg_start_dist <- cum_dist
    seg_end_dist <- cum_dist + seg_lengths[s]

    while (node_idx <= n) {
      target_dist <- (node_idx - 1) * spacing
      if (target_dist >= seg_start_dist && target_dist < seg_end_dist) {
        frac <- (target_dist - seg_start_dist) / seg_lengths[s]
        x[node_idx] <- vx[s] + frac * (vx[next_s] - vx[s])
        y[node_idx] <- vy[s] + frac * (vy[next_s] - vy[s])
        node_idx <- node_idx + 1
      } else {
        break
      }
    }
    cum_dist <- seg_end_dist
  }

  list(x = x, y = y)
}

# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a
