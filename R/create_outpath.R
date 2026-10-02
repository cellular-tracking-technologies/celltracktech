#' Create Outpath for CTT data
#'
#' @param outpath a string of the directory or nested directories where you want to save your data
#'
#' @returns a string of the full directory path
#' @export
#'
#' @examples
#' create_outpath('./data/')
#' create_outpath('./data/meadows/')
create_outpath <- function(outpath) {
  # get current working directory
  project_dir <- getwd()

  # build full path
  full_path <- file.path(project_dir, outpath)

  # create outpath directory or directories if they do not exist
  if (file.exists(full_path)) {
    message(paste('Folder exists, no need to create a new directory.'))
  } else {
    # create a new sub directory inside the main path
    message(paste('Folder', outpath, 'does not exist, creating it now.'))
    dir.create(full_path, recursive = TRUE)
  }

  return(full_path)
}
