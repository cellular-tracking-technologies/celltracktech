get_file_info <- function(e) {
  y <- basename(e)
  splitfile <- unlist(strsplit(y, "CTT-"))
  fileinfo <- splitfile[2]
  sensorid <- unlist(strsplit(fileinfo, "-"))
  sensor <- sensorid[1]
  filenameinfo <- sensorid[2]
  file_info <- unlist(strsplit(filenameinfo, "\\."))[1]
  filetype <- ifelse(is.na(as.integer(file_info)), file_info, "sensorgnome") # this throws a noisy warning message, smooth out?
  if (is.na(filetype)) {
    filetype <- "none"
  } else if (filetype == "node" & !is.na(filetype)) {
    filetype <- "node_health"
  } else if (filetype == "data") {
    filetype <- "raw"
  }
  thisfile <- list(filetype = filetype, sensor = sensor, y = y)
  return(thisfile)
}
