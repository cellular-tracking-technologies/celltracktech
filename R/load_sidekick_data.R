load_sidekick_data <- function(sidekick_file_path) {
    df <- read.csv(sidekick_file_path)

    num_cols <- ncol(df)
    print(num_cols)

    if (num_cols == 8) {
        colnames(df) <- c("tag_type", "tag_id", "time_utc", "rssi", "lat", "lon", "heading", "antenna_angle")
    } else if (num_cols == 11) {
        colnames(df) <- c("tag_type", "tag_id", "time_utc", "rssi", "lat", "lon", "sync", "tag_family", "payload_type", "solar_mV", "temperature_C")
    }
    # Remove the microseconds
    df <- transform(df, time_utc = substring(c(time_utc), 1, 19))
    # Convert to date object
    df <- transform(df, time_utc = as.POSIXct(time_utc, tz = "GMT"))

    return(df)
}
