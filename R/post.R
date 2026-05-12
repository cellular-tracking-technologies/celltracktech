post <- function(endpoint, payload = NULL, show_progress = FALSE) {
  payload_to_send <- list(token = my_token)
  if (!is.null(payload)) {
    payload_to_send <- c(payload_to_send, payload)
  }
  if (show_progress) {
    response <- httr::POST(host, path = endpoint, body = payload_to_send, encode = "json", httr::timeout(60), httr::progress())
  } else {
    response <- httr::POST(host, path = endpoint, body = payload_to_send, encode = "json", httr::timeout(60))
  }
  if (httr::status_code(response) == 503) {
    warning("Server query timeout \u2014 try narrowing your date range or using pagination")
  }
  httr::stop_for_status(response)
  return(response)
}
