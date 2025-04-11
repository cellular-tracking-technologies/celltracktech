# library(mapview)
source("R/functions/utils/rssi_v_dist.R")
source("R/functions/utils/get_time_value.R")
source("R/functions/grid_search/calc_receiver_values.R")
source("R/functions/grid_search/calc_grid_values.R")
source("R/functions/grid_search/map_grid_solution.R")

#' Title
#'
#' @param start_time 
#' @param length_seconds 
#' @param step_size_seconds 
#' @param det_time_window 
#' @param filter_alpha 
#' @param filter_time_range 
#' @param grid_df 
#' @param detection_df 
#' @param node_locs 
#' @param node_t_offset 
#' @param rssi_coefs 
#' @param track_frame_output_path 
#' @param tile_url 
#'
#' @returns
#' @export
#'
#' @examples
calculate_track <- function(
    start_time,
    length_seconds,
    step_size_seconds,
    det_time_window,
    filter_alpha,
    filter_time_range,
    grid_df,
    detection_df,
    node_locs,
    node_t_offset = NULL,
    rssi_coefs,
    track_frame_output_path = NULL,
    tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {
    #################################################################
    num_steps <- length_seconds / step_size_seconds
    start_time_value <- get_time_value(start_time)

    track_df <- data.frame(
        i = integer(),
        time = integer(),
        lat = double(),
        lon = double(),
        max_rssi = double(),
        avg_rssi = double(),
        ml_lat = double(),
        ml_lon = double(),
        error = double()
    )

    for (i in 1:num_steps) {
        print(paste("Starting: ", i, "/", num_steps))
        # Get the time range for this bin
        #  -Stop at the current bin time
        #  -Take all detections within one time_window prior to current bin time
        bin_stop_value <- start_time_value + (i - 1) * step_size_seconds
        # bin_start_value <- bin_stop_value - det_time_window

        # For all detections in this bin calculate the avg rssi in each receiver
        print("Calculating receiver values...")
        rec_df <- calc_receiver_values(
            current_time = bin_stop_value,
            det_window = det_time_window,
            station_tag_df = detection_df,
            node_locs = node_locs,
            node_t_offset,
            rssi_coefs = rssi_coefs,
            filter_alpha = filter_alpha,
            filter_time_range = filter_time_range
        )
        # print(rec_df)

        ##################################
        reduced_rec_df <- subset.data.frame(rec_df, 
                                            rec_df$filtered_rssi >= rssi_coefs[1])
        
        # if filtered rec_df has only 1 row, skip and move on with for loop
        if (nrow(reduced_rec_df) < 2) next
        
        node_w_max <- reduced_rec_df[reduced_rec_df$filtered_rssi == max(reduced_rec_df$filtered_rssi),]
        
        # if filtered_rssi values match, leads to two rows or more rows, arrange by latest time, then obtain first row
        if (nrow(node_w_max) > 1) {
          node_w_max <- node_w_max %>%
            arrange(desc(latest_time)) %>%
            filter(row_number()==1)
        }
        
        # issue with reduced_rec_df being 0 or 1 rows long, would break function, but have not encountered it yet so will comment out
        # if (nrow(reduced_rec_df) < 2) next

        list_exp_dist = reduced_rec_df[['exp_dist']]
        lat = reduced_rec_df[['lat']]
        lon = reduced_rec_df[['lon']]
        
        multilat_fit <- nls(list_exp_dist ~ haversine(lat,
                                                      lon,
                                                      ml_lat,
                                                      ml_lon
                                                      ),
                      data = reduced_rec_df,
                      start = list(ml_lat = node_w_max$lat, ml_lon = node_w_max$lon),
                      control = nls.control(warnOnly = T, 
                                            minFactor=1/65536, 
                                            maxiter = 100)
                    )

        co <- coef(summary(multilat_fit))
        ##################################


        # Check that at least three nodes have detections in time window
        nodes_with_dets <- sum(rec_df$n > 0)
        if (nodes_with_dets >= 3) {
            # Calculate the "grid value" for each spatial bin in the grid
            print("Calculating grid values...")
            grid_values <- calc_grid_values(grid_df, rec_df, rssi_coefs)

            # Find the bin with best "grid value"
            solution <- subset(grid_values, grid_values$value == max(grid_values$value))

            # Add solution to the track
            track_point <- data.frame(
                i = i,
                time = bin_stop_value,
                lat = solution$center_lat,
                lon = solution$center_lon,
                max_rssi = max(rec_df$avg_rssi),
                avg_rssi = mean(rec_df$avg_rssi),
                ml_lat = co[1,1],
                ml_lon = co[2,1],
                error = NaN
            )
            track_df <- rbind(track_df, track_point)

            # Only draw and export the individual frame if a output path was specified
            # library(mapview) is required to execute this code and save individual time step images
            # if (!is.null(track_frame_output_path)) {
            #     print("Drawing map...")
            #     map <- map_latest_solution(node_locs, rec_df, grid_values, solution, track_df, tile_url)
            #     # Save the map to a png
            #     print("Exporting map...")
            #     map_file_path <- paste(track_frame_output_path, "t", i, ".png", sep = "")
            #     mapshot(map, file = map_file_path)
            # }
        } else {
            print("Skipping time bin due to not enough detections")
        }
    }

    print("Track calulcation complete!!!")
    return(track_df)
}
