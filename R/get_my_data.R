#' Download data
#'
#' This function allows you to download your sensor station data, with the option of simultaneously importing to a local database
#' @param my_token your API key
#' @param outpath where your files are to be downloaded
#' @param db_name (optional) the connection to your local database
#' @param myproject the name of your project on our system
#' @param mystation (optional) the station ID you'd like to download data from
#' @param begin (optional) limit your data download to a start time
#' @param end (optional) limit your data download to an end time
#' @export
#' @examples
#' get_my_data(token, "~/mydata", myproject = "Project Name from CTT Account")
get_my_data <- function(my_token,
                        outpath,
                        db_name = NULL,
                        myproject = NULL,
                        mystation = NULL,
                        begin = NULL,
                        end = NULL,
                        filetypes=NULL) {

  projects <- project_list(my_token, myproject)

  if (!is.null(db_name) & length(grep("postgresql", format(db_name))) > 0) {
    create_db(db_name) # EDIT TO TAKE NEW create_db() when you switch back!
    sapply(projects, pop_proj, conn = db_name)
    failed <- lapply(projects, get_data, f = db_name, outpath = outpath, my_station = mystation, beginning = begin, ending = end, filetypes=filetypes)
  } else if(!is.null(db_name) & length(grep("duckdb", format(db_name))) > 0) {
    message('projects')
    message(paste0("projects found: ", length(projects)))
    create_duck(db_name)
    sapply(projects, pop_proj, conn = db_name)
    failed <- lapply(projects, get_data, f = db_name, outpath = outpath, my_station = mystation, beginning = begin, ending = end, filetypes=filetypes)
  } else {
    failed <- lapply(projects, get_data, outpath = outpath, my_station = mystation, beginning = begin, ending = end, filetypes=filetypes)
  }
  message(paste('files that failed to download', failed[[1]]))
  faul <- which(!sapply(failed[[1]], is.null))
  if (length(faul > 0)) {
    failed <- Map(`[`, failed, faul)
    #save(failed, file = file.path(outpath, "caught.RData"))
  } else {
    failed <- "all good!"
    #save(failed, file = file.path(outpath, "caught.RData"))
  }
}
