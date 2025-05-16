#' Prepare Data
#'
#' @param x
#' @param y
#' @param SLIDE.TIME
#' @param GROUP.TIME
#' @param K
#' @param a
#' @param S
#' @param startval
#'
#' @returns dataframe
#' @export
#'
#' @examples
prep.data <- function(x,y, SLIDE.TIME, GROUP.TIME, K, a, S, startval = NULL) {

  # supress warnings
  options(warn = -1)

  # Sliding window over RSSI values for a given TagId and NodeId
  # 1st argument: vector to iterate over and apply function
  # 2nd argument: datetime index to break into periods
  # 3rd argument: period to group by "hour", "minute", "second"
  # function to apply
  # the number of values before or after current element to include in sliding window
  beep.slide <- x %>%
    dplyr::group_by(tag_id, node_id) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(roll.TagRSSI = slider::slide_index_dbl(tag_rssi, time, mean,
                                                         .after = lubridate::seconds(SLIDE.TIME), .before = lubridate::seconds(SLIDE.TIME))) %>%
    ungroup()


  # Average RSSI values by each minute for each TagId and NodeId
  # ignore warning
  beep.grouped <- beep.slide %>%
    dplyr::group_by(tag_id)

  if (is.null(startval)) {
    beep.grouped <- beep.grouped %>%
      padr::thicken(GROUP.TIME, colname="Time.group", by = "time") %>%
      dplyr::group_by(tag_id, node_id, Time.group) %>%
      dplyr::summarise(mean_rssi = mean(roll.TagRSSI), beep_count = length(roll.TagRSSI)) %>%
      ungroup()} else {
        beep.grouped <- beep.grouped %>%
          padr::thicken(GROUP.TIME, colname="Time.group", by = "time", start_val=startval) %>%
          dplyr::group_by(tag_id, node_id, Time.group) %>%
          dplyr::summarise(mean_rssi = mean(roll.TagRSSI), beep_count = length(roll.TagRSSI)) %>%
          ungroup()
      }

  e.dist <- (log(beep.grouped$mean_rssi - K) - log(a)) / -S
  beep.grouped$e.dist <- e.dist

  # calculate radius around a node given the exponential relationship between RSSI and distance

  # Remove data with NAs produced from e.dist
  beep.grouped <- beep.grouped[complete.cases(beep.grouped),]

  # Change negative distances to 10 (RSSI values > intercept of exponential curve and thus negative) - indicates very close to the node
  beep.grouped <- beep.grouped %>%
    dplyr::mutate(e.dist = dplyr::case_when(e.dist < 0 ~ 10,
                                            e.dist >=0 ~ e.dist))

  # Add Node UTMs to data
  beep.grouped <- beep.grouped %>%
    dplyr::left_join(y[,c("node_id", "node_lng", "node_lat")])

  return(beep.grouped[order(beep.grouped$Time.group),])

}
