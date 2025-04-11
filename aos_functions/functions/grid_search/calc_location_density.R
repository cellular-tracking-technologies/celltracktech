library(dplyr)
calc_location_density <- function(grid_df, locations_df) {
    loc_density_df <- data.frame(
        i = integer(),
        lat1 = double(),
        lon1 = double(),
        lat2 = double(),
        lon2 = double(),
        center_lat = double(),
        center_lon = double(),
        count = integer()
    )

    for (i in 1:nrow(grid_df)) {
        grid_cell <- grid_df[i, ]
        min_lat <- grid_cell$lat1
        max_lat <- grid_cell$lat2
        min_lon <- grid_cell$lon1
        max_lon <- grid_cell$lon2

        cell_locs <- subset.data.frame(locations_df, locations_df$lat & lat >= min_lat & locations_df$lat <= max_lat & locations_df$lon >= min_lon & locations_df$lon <= max_lon)

        count <- nrow(cell_locs)
        # if (count > 0) {
        #     print(paste(grid_cell$i, count))
        # }

        this_cell_df <- data.frame(
            i = grid_cell$i,
            lat1 = grid_cell$lat1,
            lon1 = grid_cell$lon1,
            lat2 = grid_cell$lat2,
            lon2 = grid_cell$lon2,
            center_lat = grid_cell$center_lat,
            center_lon = grid_cell$center_lon,
            count = count
        )
        loc_density_df <- rbind(loc_density_df, this_cell_df)
    }

    return(loc_density_df)
}
