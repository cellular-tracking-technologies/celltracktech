# Shared utilities and constants for CTT API functions

correct_colnames <- function(df) {
  rowval <- gsub("^X\\.", "-", colnames(df))
  rowval <- gsub("^X", "", rowval)
  DatePattern <- "^[[:digit:]]{4}\\.[[:digit:]]{2}\\.[[:digit:]]{2}[T,\\.][[:digit:]]{2}\\.[[:digit:]]{2}\\.[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?"
  rowval[which(grepl(DatePattern, rowval))] <- as.character(as.POSIXct(rowval[grepl(DatePattern, rowval)], format = "%Y.%m.%d.%H.%M.%S", tz = "UTC"))
  return(rowval)
}

DatePattern <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[T, ][[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?"
is_posixct <- function(x) inherits(x, "POSIXct")

resave <- function(..., list = character(), file) {
  previous <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}

fixtime <- function(y) {
  if (any(grepl("T", y))) {
    vals <- as.POSIXct(y, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE)
  } else {
    vals <- y
    vals <- unname(sapply(y, function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC", optional = TRUE)))
    vals1 <- sapply(vals, function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%OS"))
    vals <- as.POSIXct(vals1, tz = "UTC")
  }
  return(vals)
}

fixrow <- function(rowlen, rowfix, e, correct, DatePattern, filetype) {
  getrow <- read.csv(e, as.is = TRUE, na.strings = c("NA", ""), header = FALSE, col.names = paste0("V", seq_len(rowlen)), skipNul = TRUE, skip = rowfix, nrow = 1, fill = TRUE)
  getrow <- getrow[, (length(getrow) - correct + 1):length(getrow)]
  getrow[, 1] <- substring(getrow[, 1], regexpr(DatePattern, getrow[, 1])) # handling assumes e.g. extra field and correct record starts in column 2
  getrow[, 1] <- fixtime(getrow[, 1])
  if (length(getrow) > 6 & filetype != "gps") {
    getrow[, 7] <- substring(getrow[, 7], regexpr(DatePattern, getrow[, 7])) # handling assumes e.g. extra field and correct record starts in column 2
    getrow[, 7] <- fixtime(getrow[, 7])
  }
  getrow[, 3] <- as.character(getrow[, 3])
  return(getrow[1, ])
}

# host <- "https://api.internetofwildlife.com/"
host <- 'http://localhost:3000/'
project <- "/station/api/projects"
stations <- "/station/api/stations/"
checkins <- "/station/api/station-checkins/"
files <- "/station/api/file-list"
file_types <- c("data", "node-data", "gps", "log", "telemetry", "sensorgnome", "ble", "blu")
