#' Calculate Receiver Values
#'
#' @param current_time POSIXct datetime value, in UTC/GMT timezone
#' @param det_window Detection time window (s)
#' @param station_tag_df Dataframe of tag detections, obtained from 'raw' data table
#' @param node_locs Node locations dataframe
#' @param node_t_offset Node time offset, obtained from Node Time Offset functions (Ch. 5)
#' @param rssi_coefs RSSI vs Distance fit coefficients from calibration
#' @param filter_alpha Alpha value used to filter RSSI values
#' @param filter_time_range Time (s), used to get detections up to current time
#'
#' @returns rec_df
#' @export
#'
#' @examples
#' test_rec_df <- calc_receiver_values(
#'     current_time = test_time,
#'     det_window = 60,
#'     station_tag_df = detection_df,
#'     node_locs = node_locs,
#'     node_t_offset = node_toff_df,
#'     rssi_coefs = rssi_coefs,
#'     filter_alpha = 0.7,
#'     filter_time_range = 120
#' )
#' calc_receiver_values(
#'     current_time,
#'     det_window,
#'     station_tag_df,
#'     node_locs,
#'     node_t_offset = NULL,
#'     rssi_coefs,
#'     filter_alpha,
#'     filter_time_range)
calc_receiver_values <- function(
    current_time,
    det_window,
    station_tag_df,
    node_locs,
    node_t_offset = NULL,
    rssi_coefs,
    filter_alpha,
    filter_time_range) {
    #########################################################
    # Get all detections up to this time

    print('station tag df')
    print(station_tag_df)

    if ('time' %in% colnames(station_tag_df)) {
      station_tag_df$time_value = station_tag_df$time
    } else if ('time_value' %in% colnames(station_tag_df)) {
      station_tag_df$time = station_tag_df$time_value
    }

    new_df <- station_tag_df %>%
        filter(station_tag_df$time_value >= current_time - filter_time_range &
               station_tag_df$time_value <= current_time)

    rec_df <- data.frame(
        node_id = character(),
        lat = double(),
        lon = double(),
        n = integer(),
        latest_time = integer(),
        avg_rssi = double(),
        filtered_rssi = double(),
        exp_dist = double(),
        stringsAsFactors = TRUE
    )

    for (n in 1:nrow(node_locs)) {
        node <- node_locs[n, ]
        this_node_id <- node$node_id
        # print('calculate receiver values node id')
        # print(this_node_id)
        if (!is.null(node_t_offset)) {
            # Get this node's time offset
            t_off <- subset.data.frame(node_t_offset,
                                       toupper(node_t_offset$node_id) == node$node_id)$mean_time_offset

            # Use time offset to adjust time window for detection cut
            new_df <- station_tag_df %>%
                filter(station_tag_df$time_value - t_off >= current_time - filter_time_range & station_tag_df$time_value - t_off <= current_time)
        }

        this_node_detections <- subset.data.frame(new_df, new_df$node_id == this_node_id)

        most_recent_detection_time <- 0
        avg_rssi <- NaN
        filtered_rssi <- NaN
        exp_dist <- NaN
        n <- nrow(this_node_detections)
        if (n > 0) {
            this_node_detections <- this_node_detections[order(this_node_detections$time_value), ]
            most_recent_detection_time <- max(this_node_detections$time_value)
            avg_rssi <- mean(this_node_detections$tag_rssi)
            # Apply low pass filter to incoming detection RSSI's
            for (d in 1:nrow(this_node_detections)) {
                detection <- this_node_detections[d, ]
                if (d == 1) {
                    filtered_rssi <- detection$tag_rssi
                } else {
                    filtered_rssi <- filter_alpha * filtered_rssi + (1 - filter_alpha) * detection$tag_rssi
                }
            }
            exp_dist <- predict_dist(rssi_coefs, filtered_rssi)
        }

        # Check to see if latest detection is within the detection time window
        if (most_recent_detection_time >= current_time - det_window) {
            single_node <- data.frame(
                node_id = this_node_id,
                lat = node$avg_lat,
                lon = node$avg_lon,
                n = n,
                latest_time = most_recent_detection_time,
                avg_rssi = avg_rssi,
                filtered_rssi = filtered_rssi,
                exp_dist = exp_dist
            )
            rec_df <- rbind(rec_df, single_node)
        }
    }

    return(rec_df)
}
