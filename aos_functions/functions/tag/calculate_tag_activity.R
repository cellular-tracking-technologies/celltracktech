library(dplyr)

calculate_tag_activity <- function(tag_dets, tag_beep_interval) {
    start_time <- Sys.time()
    tag_activity <- data.frame(
        node_id = character(),
        time = as.POSIXct(character()),
        activity = integer()
    )

    tag_dets[order(tag_dets$time, decreasing = FALSE), ]

    nodes <- unique(tag_dets$node_id)

    for (i in seq_along(nodes)) {
        node_tag_dets <- subset.data.frame(tag_dets, tag_dets$node_id == nodes[i])
        node_tag_dets$prev_time <- as.POSIXct(c(NA, head(node_tag_dets$time, -1)))
        node_tag_dets$prev_rssi <- c(NA, head(node_tag_dets$tag_rssi, -1))

        node_tag_dets <- node_tag_dets %>% mutate(delta_time = node_tag_dets$time - node_tag_dets$prev_time)
        node_tag_dets <- node_tag_dets %>% mutate(activity = node_tag_dets$tag_rssi - node_tag_dets$prev_rssi)

        node_tag_dets <- subset.data.frame(node_tag_dets, node_tag_dets$delta_time <= 2 * tag_beep_interval)
        node_tag_dets <- node_tag_dets %>% mutate(act_time = node_tag_dets$time - node_tag_dets$delta_time / 2)
        
        result <- data.frame(node_id = node_tag_dets$node_id, time = node_tag_dets$act_time, activity = node_tag_dets$activity, abs_act = abs(node_tag_dets$activity))
        
        tag_activity <- rbind(tag_activity, result)
    }
    end_time <- Sys.time()
    print(paste("Calculation time = ", end_time - start_time, "seconds"))
    return(tag_activity)
}

calc_avg_activity <- function(tag_activity, start_time, stop_time) {
    result <- data.frame(
        time = as.POSIXct(character()),
        avg_activity = double(),
        sd_activity = double()
    )

    t <- start_time
    t_bin_size <- 3600
    while (t < stop_time) {
        time_range_data <- subset.data.frame(tag_activity, tag_activity$time >= t)
        time_range_data <- subset.data.frame(time_range_data, time_range_data$time < t + t_bin_size)

        avg <- mean(time_range_data$abs_act)
        sd <- sd(time_range_data$abs_act)

        df <- data.frame(
            time = t + t_bin_size / 2,
            avg_activity = avg,
            sd_activity = sd
        )
        result <- rbind(result, df)

        t <- t + t_bin_size
    }

    return(result)
}
