#' Make the node file
#'
#' This function allows you to create the node location file from your node health data.
#' @param health a data frame of node health data pulled from your local database
#' @export

node_file <- function(health) {
  if (nrow(health) < 1) stop("no node health data!")
  health$timediff <- as.integer(health$time - health$recorded_at)
  #health <- health[health$timediff == 0,]
  health <- aggregate(health[,c("latitude", "longitude")],list(health$node_id), median, na.rm=TRUE)
  if (any(is.na(health))) {health <- health[-which(is.na(health$latitude) | is.na(health$latitude)),]}
  #
  colnames(health)[colnames(health)=="latitude"] <- "lat"
  colnames(health)[colnames(health)=="longitude"] <- "lng"
  colnames(health)[colnames(health)=="Group.1"] <- "NodeId"
  wells_sf <- sf::st_as_sf(health, coords = c("lng","lat"), crs=4326)
  return(wells_sf)}
