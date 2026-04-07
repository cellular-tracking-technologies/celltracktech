#' Get list of projects associated with your account
#'
#' @param my_token API authentication token
#' @param myproject optional project name to filter by
#' @return list of projects, or a single project if myproject is specified
#' @export
get_my_projects <- function(my_token, myproject = NULL) {
  projects <- httr::content(httr::POST(host, path = project, body = list(token = my_token), encode = "json"))
  message(paste('projects', projects))
  projects <- projects[["projects"]]
  message(projects)
  if (!is.null(myproject)) {

    projects <- tryCatch({
      message(paste('The project name you entered is:', myproject))

      list(projects[[which(sapply(projects, function(x) x[["name"]]) == myproject)]])
    }, error = function(err) {
      message(paste('Error:', conditionMessage(err)))
      cat('The project you entered is not found in your project list. Check your spelling and if you have access to the project.\n')
    })
    message(projects)
  }
  return(projects)
}
