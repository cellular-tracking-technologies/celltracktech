## RSSI vs Distance relationship:
## RSSI = a - b * exp(-c * DISTANCE)
## rssi_coefs = [a,b,c]

#' RSSI Coefficients
#'
#' @param rssi_coefs
#' @param dist
#'
#' @returns rssi_coefs
#' @export
#'
#' @examples
predict_rssi <- function(rssi_coefs, dist) {
    return(rssi_coefs[1] - rssi_coefs[2] * exp(-rssi_coefs[3] * dist))
}

#' Predict Distance
#'
#' @param rssi_coefs
#' @param rssi
#'
#' @returns rssi_coefs
#' @export
#'
#' @examples
predict_dist <- function(rssi_coefs, rssi) {
    if (rssi <= rssi_coefs[1]) {
        return(1000)
    } else {
        return(-(1 / rssi_coefs[3]) * log((rssi_coefs[1] - rssi) / rssi_coefs[2]))
    }
}
