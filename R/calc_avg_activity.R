#' Title
#'
#' @param tag_activity
#' @param start_time
#' @param stop_time
#'
#' @returns result
#' @export
#'
#' @examples
#' calc_avg_activity(tag_activity, start_time, stop_time)
#'
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
