build_grid <- function(node_locs, 
                       center_lat, 
                       center_lon, 
                       x_size_meters, 
                       y_size_meters, 
                       bin_size) {
    get_delta_lat <- function(lat, dist) {
        pi <- 3.1415926535897932
        earth_radius <- 6371.0e3 # meters
        lat1_rad <- lat * pi / 180.0
        angular_dist <- dist / earth_radius
        lat2_rad <- asin(sin(lat1_rad) * cos(angular_dist) + cos(lat1_rad) * sin(angular_dist))
        lat2_deg <- lat2_rad * 180 / pi
        return(lat2_deg - center_lat)
    }

    get_delta_lon <- function(lat, dist) {
        pi <- 3.1415926535897932
        earth_radius <- 6371.0e3 # meters
        lat1_rad <- lat * pi / 180.0
        angular_dist <- dist / earth_radius
        lon2_rad <- atan2(sin(angular_dist) * cos(lat1_rad), cos(angular_dist) - sin(lat1_rad) * sin(lat1_rad))
        lon2_deg <- lon2_rad * 180 / pi
        return(lon2_deg)
    }

    delta_lon <- get_delta_lon(center_lat, x_size_meters / 2)
    delta_lat <- get_delta_lat(center_lat, y_size_meters / 2)

    num_bins_x <- x_size_meters / bin_size
    num_bins_y <- y_size_meters / bin_size

    grid_data_frame <- data.frame(
        i = integer(),
        lat1 = double(),
        lon1 = double(),
        lat2 = double(),
        lon2 = double(),
        center_lat = double(),
        center_lon = double()
    )

    starting_lat <- center_lat - delta_lat
    starting_lon <- center_lon - delta_lon
    d_lat <- get_delta_lat(center_lat, bin_size)
    d_lon <- get_delta_lon(center_lat, bin_size)

    bin_count <- 1
    for (iy in 1:num_bins_y) {
        for (ix in 1:num_bins_x) {
            lat1 <- starting_lat + (iy - 1) * d_lat
            lon1 <- starting_lon + (ix - 1) * d_lon

            grid_bin <- data.frame(
                i = bin_count,
                lat1 = lat1,
                lon1 = lon1,
                lat2 = lat1 + d_lat,
                lon2 = lon1 + d_lon,
                center_lat = lat1 + d_lat / 2,
                center_lon = lon1 + d_lat / 2
            )
            grid_data_frame <- rbind(grid_data_frame, grid_bin)
            bin_count <- bin_count + 1
        }
    }
    return(grid_data_frame)
}
