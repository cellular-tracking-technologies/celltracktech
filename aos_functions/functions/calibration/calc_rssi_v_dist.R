source("R/functions/utils/haversine.R")

calc_rssi_v_dist <- function(node_locs, sidekick_tag_df, detection_df, use_sync = TRUE) {
    result <- data.frame(
        node_id = character(),
        distance = double(),
        rssi = integer(),
        stringsAsFactors = TRUE
    )

    # Amount of time (seconds) to allow node and sidekick time to differ
    time_window <- 1

    for (r in 1:nrow(sidekick_tag_df)) {
        sidekick_beep <- sidekick_tag_df[r, ]
        # print(sidekick_beep)
        matching_beeps <- detection_df
        if (use_sync == TRUE) {
            matching_beeps <- subset.data.frame(
                matching_beeps,
                matching_beeps$Sync == sidekick_beep$sync
            )
            # print(matching_beeps)
        }
        lower_time_limit <- sidekick_beep$time_utc - time_window
        upper_time_limit <- sidekick_beep$time_utc + time_window
        matching_beeps <- subset.data.frame(
            matching_beeps,
            matching_beeps$time >= lower_time_limit & matching_beeps$time <= upper_time_limit
        )
        # print(matching_beeps)
        
        if (nrow(matching_beeps) > 0) {
            for (i in 1:nrow(matching_beeps)) {
                beep <- matching_beeps[i, ]
                beep_node_id <- beep$node_id
                rssi <- beep$tag_rssi
                # print(beep$NodeId)
                beep_node_loc <- subset.data.frame(
                    node_locs,
                    node_locs$node_id == beep_node_id
                )

                distance <- haversine(sidekick_beep$lat, sidekick_beep$lon, beep_node_loc$avg_lat, beep_node_loc$avg_lon)
                # print(distance)
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
    # print(nrow(result))
    return(result)
}
