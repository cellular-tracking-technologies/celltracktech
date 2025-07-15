#' Get Tag Detections
#'
#' @param detection_df Tag detection dataframe
#' @param min_det_count Minimum number of detections to include
#'
#' @returns tag_counts - dataframe
#' @export
#'
#' @examples
#' get_tag_detection_count(detection_df, min_det_count)
#'
#'
get_tag_detection_count <- function(detection_df,
                                    min_det_count) {

    tag_counts <- detection_df %>% count(tag_id)
    tag_counts <- subset.data.frame(tag_counts, tag_counts$n > min_det_count)
    tag_counts <- tag_counts[order(tag_counts$n, decreasing = TRUE), ]

    return(tag_counts)
}
