## RSSI vs Distance relationship:
## RSSI = a - b * exp(-c * DISTANCE)
## rssi_coefs = [a,b,c]

#' RSSI Coefficients
#'
#' @param rssi_coefs Numeric vector of RSSI model coefficients (a, b, c)
#' @param dist Distance value
#'
#' @returns rssi_coefs
#' @export
#'
#' @examples
#' \dontrun{
#' predict_rssi(c(-95, 60, 0.05), 100)
#' }
predict_rssi <- function(rssi_coefs, dist) {
    return(rssi_coefs[1] - rssi_coefs[2] * exp(-rssi_coefs[3] * dist))
}

#' Predict Distance
#'
#' @param rssi_coefs Numeric vector of RSSI model coefficients (a, b, c)
#' @param rssi RSSI value
#'
#' @returns rssi_coefs
#' @export
#'
#' @examples
#' \dontrun{
#' predict_dist(c(-95, 60, 0.05), -80)
#' }
predict_dist <- function(rssi_coefs, rssi) {
    if (rssi <= rssi_coefs[1]) {
        return(1000)
    } else {
        return(-(1 / rssi_coefs[3]) * log((rssi_coefs[1] - rssi) / rssi_coefs[2]))
    }
}
