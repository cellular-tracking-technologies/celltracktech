#' Create Outpath for CTT data
#'
#' @param outpath a string of the directory or nested directories where you want to save your data
#'
#' @returns creates directory/directories in your current working directory
#' @export
#'
#' @examples
#' create_outpath('./data/')
#' create_outpath('./data/meadows/')
create_outpath <- function(outpath) {
  # get current working directory
  project_dir <- getwd()

  # create outpath directory or directories if they do not exist
  if (file.exists(outpath)) {
    print(paste('Folder exists, no need to create a new directory.'))
  } else {
    # create a new sub directory inside the main path
    print(paste('Folder', outpath, 'does not exist, creating it now.'))
    dir.create(file.path(project_dir, outpath), recursive = TRUE)
  }
}
