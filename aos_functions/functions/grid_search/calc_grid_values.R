library(dplyr)

source("R/functions/utils/haversine.R")

calc_grid_values <- function(grid_df, rec_df, rssi_coefs) {
    calc_bin_value <- function(bin_lat, bin_lon, rec_df, rssi_coefs) {
        total_diff2 <- 0
        for (n in 1:nrow(rec_df)) {
            rec <- rec_df[n, ]
            if (rec$n > 0) {
                dist <- haversine(rec$lat, rec$lon, bin_lat, bin_lon)
                expected_rssi <- predict_rssi(rssi_coefs, dist)
                # diff2 <- (rec$avg_rssi - expected_rssi) * (rec$avg_rssi - expected_rssi)
                diff2 <- (rec$filtered_rssi - expected_rssi) * (rec$filtered_rssi - expected_rssi)
                total_diff2 <- total_diff2 + diff2
            }
        }
        return(1 / total_diff2)
    }
    grid_value_data_frame <- grid_df %>% mutate(value = calc_bin_value(grid_df$center_lat, grid_df$center_lon, rec_df, rssi_coefs))
    return(grid_value_data_frame)
}
