#' Node File
#'
#' @param health
#'
#' @returns health - dataframe
#' @export
#'
#' @examples
node_file <- function(health) {
  options(digits=9)

  if (nrow(health) < 1) stop("no node health data!")
  #health$timediff <- as.integer(health$time - health$recorded_at)
  #health <- health[health$timediff == 0,]
  health <- aggregate(health[,c("latitude", "longitude")],
                      list(health$node_id),
                      mean, na.rm=TRUE)

  if (any(is.na(health))) {
    health <- health[-which(is.na(health$latitude) || is.na(health$longitude)),]
  }

  colnames(health)[colnames(health)=="latitude"] <- "node_lat"
  colnames(health)[colnames(health)=="longitude"] <- "node_lng"
  colnames(health)[colnames(health)=="Group.1"] <- "node_id"
  return(health)
}
