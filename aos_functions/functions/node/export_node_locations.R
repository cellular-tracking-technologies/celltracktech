export_node_locations <- function(
    output_file_path,
    node_locs) {
    header_string <- "node_id,lat,sd_lat,lon,sd_lon"
    get_line <- function(node_loc) {
        print(node_loc)
        line_string <- sprintf(
            "%s,%s,%s,%s,%s",
            node_loc["node_id"],
            node_loc["avg_lat"],
            node_loc["sd_lat"],
            node_loc["avg_lon"],
            node_loc["sd_lon"]
        )
        return(line_string)
    }

    lines <- apply(node_locs, 1, get_line)
    lines <- c(header_string, lines)
    file_conn <- file(output_file_path)
    writeLines(lines, file_conn)
    close(file_conn)
}
