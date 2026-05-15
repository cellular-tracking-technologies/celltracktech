get_station_file_list <- function(station_id, begin, filetypes = NULL,
                                end = NULL, limit = NULL, offset = NULL) {
  endpoint <- files
  payload <- list("station-id" = station_id, begin = as.Date(begin))
  # message('payload', payload)
  if (!is.null(filetypes)) {
    add_types <- filetypes[filetypes %in% file_types]
    if (length(which(!filetypes %in% file_types)) > 0) {
      warning(paste("WARNING: invalid file type specified - ignoring:", filetypes[!filetypes %in% file_types]))
    }
    payload[["file-types"]] <- add_types
  }
  if (!is.null(end)) {
    payload[["end"]] <- as.Date(end)
  }
  if (!is.null(limit)) {
    payload[["limit"]] <- as.integer(limit)
  }
  if (!is.null(offset)) {
    payload[["offset"]] <- as.integer(offset)
  }
  return(httr::content(post(endpoint = endpoint, payload = payload)))
}
