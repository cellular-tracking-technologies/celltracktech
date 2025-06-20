#' Haversine
#'
#' @param lat1 - latitude of point 1
#' @param lon1 - longitude of point 1
#' @param lat2 - latitude of point 2
#' @param lon2 - longitude of point 2
#'
#' @returns d - great circle distance bewteen two points
#' @export
#'
#' @examples
#' haversine(reduced_rec_df$lat,
#'           reduced_rec_df$lon,
#'           ml_lat,
#'           ml_lon)
#'
haversine <- function(lat1, lon1, lat2, lon2) {
  # print(paste('lat1', lat1, 'lon1', lon1, 'lat2', lon2))
    pi <- 3.1415926535897932
    earth_radius <- 6371.0e3 # meters
    phi1 <- lat1 * pi / 180.0 # φ, λ in radians
    phi2 <- lat2 * pi / 180.0
    delta_phi <- (lat2 - lat1) * pi / 180.0
    delta_lambda <- (lon2 - lon1) * pi / 180.0

    a <- sin(delta_phi / 2) * sin(delta_phi / 2) + cos(phi1) * cos(phi2) * sin(delta_lambda / 2) * sin(delta_lambda / 2)

    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    d <- earth_radius * c # in meters
    # print(paste('d', d))
    return(d) # meters
}
