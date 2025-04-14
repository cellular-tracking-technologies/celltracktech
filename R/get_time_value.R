get_time_value <- function(time_string) {
    time_posix <- as.POSIXct(
        time_string,
        tz = "GMT",
        format = "%Y-%m-%d %H:%M:%S"
    )
    time_value <- as.integer(time_posix)
    return(time_value)
}
