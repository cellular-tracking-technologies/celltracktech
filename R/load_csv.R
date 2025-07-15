#' Function to load CSV or CSV.GZ file
#'
#' @param file_path
#'
#' @returns data
#' @export
#'
#' @examples
#' load_csv('./data/)
#' load_csv('./data/blu-file.csv')
#' load_csv('./data/blu-file.csv.gz')
load_csv <- function(file_path) {
  if (grepl("\\.gz$", file_path)) {
    # If the file is compressed (CSV.GZ), use gzfile()
    data <- read.csv(gzfile(file_path))
  } else {
    # Otherwise, load it directly as a CSV
    data <- read.csv(file_path)
  }
  return(data)
}
