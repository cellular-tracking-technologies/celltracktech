get_checkins <- function(
    my_token,
    station_id,
    number) {
  endpoint <- checkins
  payload <- list(
    "token" = my_token,
    "station-id" = station_id,
    "limit" = as.integer(number)
    )
  message('payload', payload)

  if (!is.null(my_token)) {
    payload[["token"]] <- as.character(my_token)
  }
  if (!is.null(station_id)) {
    payload[["station_id"]] <- as.character(station_id)
  }
  if (!is.null(number)) {
    payload[["limit"]] <- as.integer(number)
  }

  response <- tryCatch({
    post(
      endpoint = endpoint,
      payload = payload,
      show_progress = TRUE
    )
  }, error = function(cond){
    message("Here's the original error message:")
    message(conditionMessage(cond))
    payload <- c(payload, "bypass-encoding" = "plain")
    # Choose a return value in case of error
    return(post(endpoint = endpoint, payload = payload, show_progress = FALSE))
  })

  json = toJSON(httr::content(post(endpoint = endpoint, payload = payload)), pretty = TRUE)

  return(json)
}
