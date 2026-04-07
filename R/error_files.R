#' Find files with errors
#'
#' This function allows you to copy files with errors or corrupt lines into a new destination to look closer
#' @param dirin where your files were downloaded
#' @param dirout where you want the files with errors to be copied
#' @export
#' @examples
#' error_files("~/mydata", "~/errorfiles")
error_files <- function(dirin, dirout, conn = NULL) {
  dir.create(file.path(dirout), showWarnings = FALSE)
  myfiles <- list.files(dirin, recursive = TRUE, full.names = TRUE)
  output <- file.path(dirout, "output.txt")
  fileConn <- file(output, open = "wt")
  filetest <- sapply(myfiles, function(e) {
    message(e)
    fileinfo <- get_file_info(e)
    if (fileinfo$filetype %in% c("raw", "node_health", "gps")) {
      testerr <- file_handle(e, fileinfo$filetype)
      cat(c(testerr[[3]], e), file = fileConn, append = T)
      cat("\n", file = fileConn, append = TRUE)
      testerr <- testerr[[2]]
      if (!is.null(conn)) {
        if (testerr > 0) {
          message("deleting")
          DBI::dbExecute(conn, paste0("delete from ", fileinfo$filetype, " where path = '", fileinfo$y, "'"))
          DBI::dbExecute(conn, paste0("delete from data_file where path = '", fileinfo$y, "'"))
          # z <- db_insert(contents, filetype, conn, sensor, y, begin)
        }
      }
    } else {
      testerr <- 0
    }
    return(testerr)
  })
  close(fileConn)
  missingheader <- names(which(filetest == 1))
  emptyfile <- names(which(filetest == 2))
  longrow <- names(which(filetest == 3))
  shortrow <- names(which(filetest == 4))
  rowerr <- names(which(filetest == 5))
  dir.create(file.path(dirout, "missing_header"), showWarnings = FALSE)
  file.copy(missingheader, file.path(dirout, "missing_header"))
  dir.create(file.path(dirout, "empty"), showWarnings = FALSE)
  file.copy(emptyfile, file.path(dirout, "empty"))
  dir.create(file.path(dirout, "restart_row"), showWarnings = FALSE)
  file.copy(longrow, file.path(dirout, "restart_row"))
  dir.create(file.path(dirout, "abbrev_row"), showWarnings = FALSE)
  file.copy(shortrow, file.path(dirout, "abbrev_row"))
  dir.create(file.path(dirout, "row_error"), showWarnings = FALSE)
  file.copy(rowerr, file.path(dirout, "row_error"))
  return(filetest)
}
