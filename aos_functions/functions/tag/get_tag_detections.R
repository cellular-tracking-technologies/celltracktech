library(dplyr)

get_tag_detection_count <- function(detection_df, min_det_count) {
    tag_counts <- detection_df %>% count(tag_id)
    tag_counts <- subset.data.frame(tag_counts, tag_counts$n > min_det_count)
    tag_counts <- tag_counts[order(tag_counts$n, decreasing = TRUE), ]
    return(tag_counts)
}
