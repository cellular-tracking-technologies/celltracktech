#' Function to load CSV or CSV.GZ file
#'
#' @param file_path - string
#'
#' @returns data
#' @export
#'
#' @examples
#' load_csv('./data/')
#' load_csv('./data/blu-file.csv')
#' load_csv('./data/blu-file.csv.gz')
load_csv <- function(file_path) {
  if (!file.exists(file_path) || file.size(file_path) == 0) {
    return(data.frame())
  }
  if (grepl("\\.gz$", file_path)) {
    con <- gzfile(file_path, "rt")
    first_line <- readLines(con, n = 1)
    close(con)
    if (length(first_line) == 0) return(data.frame())
    data <- read.csv(gzfile(file_path))
  } else {
    data <- read.csv(file_path)
  }
  return(data)
}
