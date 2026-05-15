#' Create database from Node Data files from SD card
#'
#' @param outpath where your files are to be downloaded
#' @param myproject the name of your project on our system
#' @param db_name the connection to your local database
#'
#' @returns database
#' @export
#'
#' @examples
#' create_database(outpath = './data/', myproject = 'Meadows V2', db_name = conn)
create_database = function(
    my_token,
    outpath,
    myproject,
    db_name) {

  # if (is.null(my_token)) {
  #   myList = list()
  #   myList$id = as.integer(1)
  #   myList$name = myproject
  #   projects = list(myList)
  # } else {
  #   projects <- project_list(my_token, myproject)
  # }

  projects <- project_list(my_token, myproject)

  # Checking if database is postgres, duckdb, or remote
  if (!is.null(db_name) && length(grep("postgresql", format(db_name))) > 0) {
    create_db(db_name) # EDIT TO TAKE NEW create_db() when you switch back!
    sapply(projects, pop_proj, conn = db_name)
    # failed <- import_node_data(conn,
    #                            outpath,
    #                            myproject=myproject)

  } else if (!is.null(db_name) && length(grep("duckdb", format(db_name))) > 0) {
    create_duck(db_name)
    sapply(projects, pop_proj, conn = db_name)

    # failed <- import_node_data(conn,
    #                            outpath,
    #                            myproject=myproject)
  } else {
    # remote database (i.e. AWS RDS)
    # failed <- import_node_data(conn,
    #                            outpath,
    #                            myproject=myproject)
  }
  # faul <- which(!sapply(failed[[1]], is.null))
  # if (length(faul > 0)) {
  #   failed <- Map(`[`, failed, faul)
  #   # save(failed, file = file.path(outpath, "caught.RData"))
  # } else {
  #   failed <- "all good!"
  #   # save(failed, file = file.path(outpath, "caught.RData"))
  # }
}
