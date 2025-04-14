## RSSI vs Distance relationship:
## RSSI = a - b * exp(-c * DISTANCE)
## rssi_coefs = [a,b,c]

predict_rssi <- function(rssi_coefs, dist) {
    return(rssi_coefs[1] - rssi_coefs[2] * exp(-rssi_coefs[3] * dist))
}

predict_dist <- function(rssi_coefs, rssi) {
    if (rssi <= rssi_coefs[1]) {
        return(1000)
    } else {
        return(-(1 / rssi_coefs[3]) * log((rssi_coefs[1] - rssi) / rssi_coefs[2]))
    }
}
