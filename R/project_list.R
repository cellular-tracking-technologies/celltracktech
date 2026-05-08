project_list <- function(my_token, myproject = NULL) {
  projects <- httr::content(httr::POST(host, path = project, body = list(token = my_token), encode = "json"))
  message(paste0('projects found: ', length(projects[["projects"]])))
  projects <- projects[["projects"]]
  if (!is.null(myproject)) {

    projects <- tryCatch({
      message(paste('The project name you entered is:', myproject))

      list(projects[[which(sapply(projects, function(x) x[["name"]]) == myproject)]])
    }, error = function(err) {
      message(paste('Error:', conditionMessage(err)))
      cat('The project you entered is not found in your project list. Check your spelling and if you have access to the project.\n')
    })
    # projects <- list(projects[[which(sapply(projects, function(x) x[["name"]]) == myproject)]])
    message(paste0("selected project: ", sapply(projects, function(x) x[["name"]])))
  }
  return(projects)
}
