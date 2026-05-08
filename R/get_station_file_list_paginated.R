#' Get all files for a station with automatic pagination
#'
#' For stations with many files, retrieves results in date-based chunks
#' to avoid server timeouts.
#'
#' @param station_id station identifier
#' @param begin start date
#' @param filetypes optional file type filter
#' @param end optional end date (defaults to today)
#' @param chunk_days number of days per request (default 30)
#' @param max_retries number of times to retry after a timeout (default 3)
#' @return list of file metadata
#' @export
get_station_file_list_paginated <- function(station_id, begin, filetypes = NULL,
                                         end = NULL, chunk_days = 30,
                                         max_retries = 3) {
  begin <- as.Date(begin)
  if (is.null(end)) {
    end <- Sys.Date()
  } else {
    end <- as.Date(end)
  }

  all_files <- list()
  chunk_start <- begin

  while (chunk_start < end) {
    chunk_end <- min(chunk_start + chunk_days, end)
    message(paste0("fetching files from ", chunk_start, " to ", chunk_end))

    result <- NULL
    for (attempt in seq_len(max_retries + 1)) {
      result <- tryCatch(
        get_station_file_list(station_id, chunk_start,
                           filetypes = filetypes, end = chunk_end),
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
    if (!is.null(batch) && length(batch) > 0) {
      all_files <- c(all_files, batch)
    }

    chunk_start <- chunk_end
  }

  message(paste0("total files retrieved: ", length(all_files)))
  return(list(files = all_files))
}
