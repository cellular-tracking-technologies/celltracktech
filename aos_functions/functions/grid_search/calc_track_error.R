source("R/functions/utils/haversine.R")

calc_track_error <- function(sidekick_df, track_df) {
    solution_compare_df <- data.frame(
        i = integer(),
        time = integer(),
        act_lat = double(),
        act_lon = double(),
        sol_lat = double(),
        sol_lon = double(),
        ml_sol_lat = double(),
        ml_sol_lon = double(),
        max_rssi = double(),
        avg_rssi = double(),
        error = double(),
        ml_error = double()
    )

    for (i in 1:nrow(track_df)) {
        track_point <- track_df[i, ]
        sidekick_same_time <- subset.data.frame(sidekick_df, sidekick_df$time_value <= track_point$time - 1)
        sidekick_same_time <- subset.data.frame(sidekick_df, sidekick_df$time_value >= track_point$time + 1)

        if (nrow(sidekick_same_time) > 0) {
            first_sidekick_beep <- sidekick_same_time[1, ]
            error <- haversine(first_sidekick_beep$lat, first_sidekick_beep$lon, track_point$lat, track_point$lon)
            ml_error <- haversine(first_sidekick_beep$lat, first_sidekick_beep$lon, track_point$ml_lat, track_point$ml_lon)

            single_solution <- data.frame(
                i = track_point$i,
                time = track_point$time,
                act_lat = first_sidekick_beep$lat,
                act_lon = first_sidekick_beep$lon,
                sol_lat = track_point$lat,
                sol_lon = track_point$lon,
                ml_sol_lat = track_point$ml_lat,
                ml_sol_lon = track_point$ml_lon,
                max_rssi = track_point$max_rssi,
                avg_rssi = track_point$avg_rssi,
                error = error,
                ml_error = ml_error
            )
            solution_compare_df <- rbind(solution_compare_df, single_solution)
        }
    }

    return(solution_compare_df)
}
