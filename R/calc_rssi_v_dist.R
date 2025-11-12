#' Calculate RSSI vs. Distance
#'
#' @param node_locs Node locations dataframe
#' @param sidekick_tag_df Sidekick tag detection dataframe
#' @param detection_df Tag detection dataframe, from 'raw' or 'blu' data table
#' @param use_sync For bluseries data, set to TRUE, for 434 MHz data set to FALSE
#'
#' @returns result
#' @export
#'
#' @examples
#' rssi_v_dist <- calc_rssi_v_dist(node_locs = node_locs,
#'                                 sidekick_tag_df = sidekick_tag_df,
#'                                 detection_df = detection_df,
#'                                 use_sync = FALSE)
#'
#'
calc_rssi_v_dist <- function(node_locs,
                             sidekick_tag_df,
                             detection_df,
                             use_sync = TRUE) {
    result <- data.frame(
        node_id = character(),
        distance = double(),
        rssi = integer(),
        stringsAsFactors = TRUE
    )

    # Amount of time (seconds) to allow node and sidekick time to differ
    time_window <- 1

    for (r in 1:nrow(sidekick_tag_df)) {
      print('row')
      print(r)
        sidekick_beep <- sidekick_tag_df[r, ]
        print('sidekick_beep')
        print(sidekick_beep)
        matching_beeps <- detection_df
        if (use_sync == TRUE) {
            matching_beeps <- subset.data.frame(
                matching_beeps,
                matching_beeps$Sync == sidekick_beep$sync
            )
        }
        lower_time_limit <- sidekick_beep$time_utc - time_window
        print('lower time limit')
        print(lubridate::ymd_hms(lower_time_limit))
        upper_time_limit <- sidekick_beep$time_utc + time_window
        print('upper time limit')
        print(lubridate::ymd_hms(upper_time_limit))
        print('first matching beeps')
        print(matching_beeps)
        matching_beeps <- subset.data.frame(
            matching_beeps,
            matching_beeps$time >= lower_time_limit & matching_beeps$time <= upper_time_limit
        )
        print('second matching beeps')
        print(matching_beeps)

        if (nrow(matching_beeps) > 0) {
            for (i in 1:nrow(matching_beeps)) {
                beep <- matching_beeps[i, ]
                beep_node_id <- beep$node_id
                rssi <- beep$tag_rssi
                # print('rssi')
                print(paste())
                beep_node_loc <- subset.data.frame(
                    node_locs,
                    node_locs$node_id == beep_node_id
                )

                distance <- haversine(sidekick_beep$lat,
                                      sidekick_beep$lon,
                                      beep_node_loc$avg_lat,
                                      beep_node_loc$avg_lon)
                # print('distance')
                # print(distance)
                print('beep node loc')
                print(tail(beep_node_loc))
                print(paste('i', i,
                            'lat', sidekick_beep$lat,
                            'lon', sidekick_beep$lon,
                            'avg lat', beep_node_loc$avg_lat,
                            'avg lon', beep_node_loc$avg_lon))
                print(paste('i', i, 'rssi', rssi, 'distance', distance))

                this_result <- data.frame(
                    node_id = beep_node_id,
                    distance = distance,
                    rssi = rssi
                )
                result <- rbind(result, this_result)
                # print(this_result)
            }
        }
    }
    print('final result')
    print(nrow(result))
    return(result)
}
