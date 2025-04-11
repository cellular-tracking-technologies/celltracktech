library(dplyr)

detection_summary <- function(detection_df, tag_list) {
    my_dets <- subset.data.frame(detection_df, detection_df$tag_id %in% tag_list)

    det_summary <- data.frame(
        tag_id = character(),
        count = integer(),
        first_det = as.POSIXct(character()),
        last_det = as.POSIXct(character()),
        duration = double()
    )

    for (i in seq_along(tag_list)) {
        this_tag_dets <- subset.data.frame(my_dets, my_dets$tag_id == tag_list[i])

        count <- nrow(this_tag_dets)
        if (count > 0) {
            first <- min(this_tag_dets$time)
            last <- max(this_tag_dets$time)
            print(paste(first, last))
            df <- data.frame(
                tag_id = tag_list[i],
                count = count,
                first_det = as.POSIXct(first),
                last_det = as.POSIXct(last),
                duration = last - first
            )
            det_summary <- rbind(det_summary, df)
        } else {
            print(paste("No detections for TagId:", tag_list[i]))
            first <- NaN
            last <- NaN
            df <- data.frame(
                tag_id = tag_list[i],
                count = count,
                first_det = first,
                last_det = last,
                duration = 0
            )
            det_summary <- rbind(det_summary, df)
        }
    }

    print(det_summary)
    return(det_summary)
}
