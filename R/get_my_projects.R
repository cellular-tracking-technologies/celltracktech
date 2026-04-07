#' Get list of projects associated with your account
#'
#' @param my_token API authentication token
#' @return character vector of project names
#' @export
get_my_projects <- function(my_token) {
  response <- httr::content(httr::POST(host, path = project, body = list(token = my_token), encode = "json"))
  projects <- response[["projects"]]
  project_names <- sapply(projects, function(x) x[["name"]])
  return(project_names)
}
