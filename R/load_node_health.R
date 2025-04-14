library(readr)
library(dplyr)
library(data.table)

load_node_health_files <- function(directory, start_time = NULL, stop_time = NULL) {
    run_start_time <- Sys.time()
    files <- list.files(path = directory, pattern = "*.csv")
    print(length(files))
    print("Finding files within time range...")
    file_df <- data.frame()
    for (f in files) {
        file <- f
        time_string <- strsplit(file, split = "[.]")[[1]][2]
        date_string <- strsplit(time_string, split = "_")[[1]][1]
        time <- as.POSIXct(date_string, "%Y-%m-%d", tz = "GMT")
        single_file_df <- data.frame(
            file_name = file,
            time = time
        )
        file_df <- rbind(file_df, single_file_df)
    }

    # Remove files outside range (kind of...)
    if (!is.null(start_time)) {
        # The time here is upload time and not the record time so allow a day buffer here
        day_before <- start_time - 86400
        file_df <- subset.data.frame(file_df, file_df$time >= day_before)
    }
    if (!is.null(stop_time)) {
        # The time here is upload time and not the record time so allow a day buffer here
        day_after <- stop_time + 86400
        file_df <- subset.data.frame(file_df, file_df$time <= day_after)
    }
    files <- file_df$file_name
    print(paste("Loading", length(files), "file(s) from", directory))

    # combine full path with file name
    files <- lapply(files, function(x) {
        return(paste(directory, x, sep = ""))
    })
    # read all of the files into a data frame
    df <- rbindlist(lapply(files, fread))
    print(paste("Finished Reading Files!", nrow(df), "total health records found."))
    print("Removing duplicates...")
    # Remove all dupiclate rows
    # (same health record but was picked up by multiple radios on the station)
    df <- df %>% distinct(NodeId, Time, RecordedAt, .keep_all = TRUE)

    print("Removing malformed records...")
    df <- subset.data.frame(df, df$Latitude <= 90.0 | is.na(df$Latitude))
    df <- subset.data.frame(df, df$Latitude >= -90.0 | is.na(df$Latitude))
    df <- subset.data.frame(df, df$Longitude <= 180.0 | is.na(df$Longitude))
    df <- subset.data.frame(df, df$Longitude >= -180.0 | is.na(df$Longitude))

    # Remove all data outside specified time ranges
    if (!is.null(start_time)) {
        df <- subset.data.frame(df, df$Time >= start_time)
    }
    if (!is.null(stop_time)) {
        df <- subset.data.frame(df, df$Time <= stop_time)
    }

    print(paste("Finished loading node health files!", nrow(df), "health records found."))
    run_end_time <- Sys.time()
    print(paste("Load time = ", run_end_time - run_start_time, "seconds"))
    return(df)
}

get_time_string <- function(file_string) {
    return(strsplit(file_string, split = "[.]")[[1]][2])
}
