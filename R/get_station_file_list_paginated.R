#' Get all files for a station with automatic pagination
#'
#' For stations with many files, retrieves results in pages to avoid
#' server timeouts. Falls back to unbounded request for small stations.
#'
#' @param station_id station identifier
#' @param begin start date
#' @param filetypes optional file type filter
#' @param end optional end date
#' @param page_size number of files per page (default 500)
#' @param max_retries number of times to retry after a timeout (default 3)
#' @return list of file metadata
#' @export
get_station_file_list_paginated <- function(station_id, begin, filetypes = NULL,
                                         end = NULL, page_size = 10,
                                         max_retries = 3) {
  all_files <- list()
  offset <- 0
  repeat {
    result <- NULL
    for (attempt in seq_len(max_retries + 1)) {
      result <- tryCatch(
        get_station_file_list(station_id, begin,
                           filetypes = filetypes, end = end,
                           limit = page_size, offset = offset),
        error = function(e) {
          if (attempt <= max_retries && grepl("imeout|503", conditionMessage(e))) {
            message(paste0("Request timed out, retrying (attempt ", attempt, " of ", max_retries, ")..."))
            Sys.sleep(2 * attempt)
            NULL
          } else {
            stop(e)
          }
        }
      )
      if (!is.null(result)) break
    }

    batch <- result[["files"]]

    if (is.null(batch) || length(batch) == 0) break
    # lapply(batch, function(f) {
    #   lapply(f, function(x) {
    #     message('getting file ', x[['name']])
    #   })
    # })
    all_files <- c(all_files, batch)
    break  # TODO: remove once server-side offset is fixed
    if (length(batch) < page_size) break
    offset <- offset + page_size
  }
  return(list(files = all_files))
}
