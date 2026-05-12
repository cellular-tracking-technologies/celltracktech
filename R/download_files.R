download_files <- function(file_id) {
  endpoint <- "/station/api/download-file/"
  payload <- list("file-id" = file_id)
  # message(paste('download_files payload', payload))

  response <- tryCatch(
    {
      post(endpoint = endpoint, payload = payload, show_progress = FALSE)
    },
    error = function(cond) {
      message("Here's the original error message:")
      message(conditionMessage(cond))
      payload <- c(payload, "bypass-encoding" = "plain")
      # Choose a return value in case of error
      post(endpoint = endpoint, payload = payload, show_progress = FALSE)
    }
  )
  return(response)
}
