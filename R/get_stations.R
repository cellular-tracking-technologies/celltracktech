get_stations <- function(project_id) {
  out <- post(endpoint = stations, payload = list("project-id" = project_id))
  return(httr::content(out))
}
