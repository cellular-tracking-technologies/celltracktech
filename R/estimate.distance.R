#' Estimate Distance
#'
#' @param x combined_data dataframe
#' @param K horizontal asymptote
#' @param a intercept
#' @param S decay factor
#'
#' @returns combined.data
#' @export
#'
#' @examples
estimate.distance <- function(x, K, a, S) {

  # supress warnings
  options(warn = -1)

  # Calculate estimated distance based on RSSI~Distance relationship and indicate simulation round
  combined.data <- x %>%
    dplyr::mutate(e.dist = (log(avgRSS - K) - log(a)) / -S)

  # Remove rows with NAs
  combined.data <- combined.data[!is.na(combined.data$e.dist),]

  # Change negative distances to 10 (rss values > intercept of exponential curve and thus negative) - indicates very close to the node
  combined.data <- combined.data %>%
    dplyr::mutate(e.dist = dplyr::case_when(e.dist < 0 ~ 10,
                                            e.dist >=0 ~ e.dist))

  # Save File to outpath
  #write.csv(combined.data, paste0(outpath, "LocError_Dataset.csv"),
  #          row.names = F)

  return(combined.data)

}
