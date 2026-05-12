file_handle <- function(e, filetype) {
  # message(paste("checking file for errors:", e))
  #print(filetype)
  file_err <- 0
  myrowfix <- c()
  ignore <- FALSE
  if(filetype=="raw") {
    contents <- tryCatch(
      {
        readr::read_csv(e, col_names = TRUE, col_types = list(NodeId="c"), show_col_types = FALSE)
      },
      error = function(err) {
        return(NULL)
      })
  } else if (filetype == 'blu') {
    process_file(e, dirname(e))

    # e_parsed = paste0(stringr::str_remove(e, '.gz'), "_parsed.csv")
    e_parsed = paste0(stringr::str_remove(e, '.gz'))
    e_gzip = R.utils::gzip(e_parsed,
                           overwrite=TRUE)
    rm(e_parsed)
    contents <- tryCatch(
      {
        readr::read_csv(e_gzip,
                        col_names = TRUE,
                        col_types = list(NodeId="c"),
                        show_col_types = FALSE)
      },
      error = function(err) {
        return(NULL)
      })
  } else {
    contents <- tryCatch(
      {
        readr::read_csv(e, col_names = TRUE, show_col_types = FALSE)
      },
      error = function(err) {
        return(NULL)
      }
    )
  }

  if (filetype == "raw" & ncol(contents) > 6) {
    contents <- contents[,1:6]
    ignore <- TRUE
  }

  if (!is.null(contents) & nrow(contents > 0)) {
    if(filetype %in% c("raw", "node_health", "gps", "blu")) {
      delete.columns <- grep("[[:digit:]]", colnames(contents), perl = T)
      if (length(delete.columns) > 0) {
        file_err <- 1
        myrowfix <- tryCatch(
          {
            myrowfix <- correct_colnames(contents)
            myrowfix[1] <- strsplit(correct_colnames(contents)[1], "[.]")[[1]][1]
            myrowfix[2] <- strsplit(correct_colnames(contents)[2], "[.]")[[1]][1]
            myrowfix[3] <- strsplit(correct_colnames(contents)[3], "\\.\\.")[[1]][1] # were there files where this wasn't correctly split?
            myrowfix[3] <- ifelse(myrowfix[3]=="", NA, myrowfix[3])
            myrowfix[4] <- strsplit(correct_colnames(contents)[4], "\\.\\.")[[1]][1]
            myrowfix[5] <- strsplit(correct_colnames(contents)[5], "\\.\\.")[[1]][1]
            if (nchar(myrowfix[5]) < 1) {
              myrowfix[5] <- NA
            }
            if (length(myrowfix) > 5) {
              myrowfix[6] <- strsplit(correct_colnames(contents)[6], "[.]")[[1]][1]
            }
            if (length(myrowfix) > 6) {
              myrowfix[7] <- strsplit(correct_colnames(contents)[7], "\\.\\.")[[1]][1]
              myrowfix[7] <- strsplit(myrowfix[7], "[_]")[[1]][1]
              myrowfix[8] <- strsplit(correct_colnames(contents)[8], "\\.\\.")[[1]][1]
            }
            if (length(myrowfix) > 9) {
              myrowfix[10] <- strsplit(correct_colnames(contents)[10], "\\.\\.")[[1]][1]
              myrowfix[10] <- ifelse(myrowfix[10]=="", NA, myrowfix[10])
              myrowfix[12] <- strsplit(correct_colnames(contents)[12], "\\.\\.")[[1]][1]
              myrowfix[13] <- strsplit(correct_colnames(contents)[13], "\\.\\.")[[1]][1]
            }
            # rowfix <- data.frame(as.POSIXct(rowfix[1], tz="UTC"), as.integer(rowfix[2]), rowfix[3], rowfix[4], rowfix[5], as.integer(rowfix[6]))
            myrowfix
            # names(rowfix) <- names(contents)
            # rbind(contents, rowfix)
          },
          error = function(err) {
            return(data.frame())
          }
        )
        # contents <- newcontents
      }

      if(!ignore & !filetype=="blu"){
        rowtest <- badrow(e, contents, filetype)
        contents <- rowtest[[1]]
      } else {
        rowtest <- list(contents,0)
        #myrowfix <- c()
      }

      if (filetype == "raw") {
        if (length(delete.columns) > 0) {
          if (ncol(contents) > 5) {
            names(contents) <- c("Time", "RadioId", "TagId", "TagRSSI", "NodeId", "Validated")
            if (length(myrowfix) > 0) {
              time <- timecheck(contents, myrowfix)
              rowfix <- data.frame(time, as.integer(myrowfix[2]), myrowfix[3], myrowfix[4], myrowfix[5], as.integer(myrowfix[6]))
              names(rowfix) <- names(contents)
              contents <- rbind(contents, rowfix)
            }
          } else {
            names(contents) <- c("Time", "RadioId", "TagId", "TagRSSI", "NodeId")
          }
        }
        # contents <- contents[(nchar(contents$NodeId) == 6 | is.na(contents$NodeId)),]
        contents = contents |>
          dplyr::filter(nchar(NodeId) == 6 | nchar(NodeId == 8) | is.na(NodeId) == TRUE)

        # correct <- ifelse(v > 2, 7, 6)
        # rowtest <- badrow(e, correct, contents)
        # contents <- rowtest[[1]]
        # if(file_err < 1) {
        #  file_err <- rowtest[[2]]
        # }
      } else if (filetype == "gps") {
        if (length(delete.columns) > 0) {
          if (ncol(contents) > 8) {
            names(contents) <- c("recorded.at", "gps.at", "latitude", "longitude", "altitude", "quality", "mean.lat", "mean.lng", "n.fixes")
            if (length(myrowfix) > 6) {
              time <- timecheck(contents, myrowfix)
              rowfix <- data.frame(time, as.POSIXct(myrowfix[2], tz = "UTC"), myrowfix[3], myrowfix[4], as.numeric(myrowfix[5]), as.numeric(myrowfix[6]), myrowfix[7], myrowfix[8], as.numeric(myrowfix[9]))
              names(rowfix) <- names(contents)
              contents <- rbind(contents, rowfix)
            }
          } else {
            names(contents) <- c("recorded.at", "gps.at", "latitude", "longitude", "altitude", "quality")
          } # not fixing rows for v1
        }
      } else if (filetype == "node_health") {
        if (length(delete.columns) > 0) {
          if (ncol(contents) > 9 & ncol(contents) < 14) {
            names(contents) <- c("Time", "RadioId", "NodeId", "NodeRssi", "Battery", "celsius", "RecordedAt", "firmware", "SolarVolts", "SolarCurrent", "CumulativeSolarCurrent", "latitude", "longitude")
            if (length(myrowfix) > 0) {
              time <- timecheck(contents, myrowfix)
              rowfix <- data.frame(time, as.integer(myrowfix[2]), myrowfix[3], as.integer(myrowfix[4]), as.numeric(myrowfix[5]), as.numeric(myrowfix[6]), as.POSIXct(myrowfix[7], tz = "UTC"), myrowfix[8], as.numeric(myrowfix[9]), as.numeric(myrowfix[10]), as.numeric(myrowfix[11]), as.numeric(myrowfix[12]), as.numeric(myrowfix[13]))
              names(rowfix) <- names(contents)
              contents <- rbind(contents, rowfix)
            }
          } else if (ncol(contents) < 9) {
            names(contents) <- c("Time", "RadioId", "NodeId", "NodeRssi", "Battery", "celsius")
          }
        }
        contents <- contents[(nchar(contents$NodeId) >= 6 & nchar(contents$NodeId) <= 8),]
      } else if(filetype=="blu") {
        #rowtest <- list(contents,0)
        if (length(delete.columns) > 0) {
          if (ncol(contents) > 8) {
            names(contents) <- c("UsbPort","BluRadioId","RadioId","Time","TagRSSI","TagId","Sync","Product","Revision","NodeId","Payload")
            rowfix <- data.frame(as.integer(myrowfix[1]), as.integer(myrowfix[2]), myrowfix[3], as.POSIXct(myrowfix[4], tz = "UTC"), as.integer(myrowfix[5]), as.character(myrowfix[6]), as.integer(myrowfix[7]), myrowfix[8], myrowfix[9], myrowfix[10], as.character(myrowfix[11]))
            names(rowfix) <- names(contents)
            contents <- rbind(contents, rowfix)
          }
        }
        contents <- contents[(nchar(contents$NodeId) <= 8 | is.na(contents$NodeId)),]
      }
      timecols <- c("Time", "recorded at", "gps at", "RecordedAt", "recorded.at", "gps.at")
      filetime <- which(names(contents) %in% timecols)
      out <- lapply(filetime, function(x) {
        timecol <- contents[, x][[1]]
        if (is.character(timecol)) {
          DatePattern <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[T, ][[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?"
          exactDatePattern <- "^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[T, ][[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?$"
          brokenrow <- grep(exactDatePattern, timecol, invert = TRUE) # find row that has a date embedded in a messed up string (i.e. interrupted rows)
          if (length(brokenrow) > 0) {
            file_err <- 6
          }
          timecol[brokenrow] <- substring(timecol[brokenrow], regexpr(DatePattern, timecol[brokenrow]))
          timecol[brokenrow[which(regexpr(DatePattern, timecol[brokenrow]) < 0)]] <- NA
          newtimecol <- as.POSIXct(timecol, tz = "UTC")
        } else {
          newtimecol <- timecol
        }
        return(newtimecol)
      })
      contents[filetime] <- out
      if ("Time" %in% colnames(contents) & nrow(contents) > 0) {
        contents <- contents[!is.na(contents$Time), ]
      }

      file_err <- ifelse(rowtest[[2]] > 0, rowtest[[2]], file_err)

      if(file_err < 5) {
        if(filetype == 'gps' & all(is.na(contents[,2]))) {file_err <- 7}
      }
      # print(contents)
    }} else {file_err <- 2}
  # print(tail(contents))
  return(list(contents, file_err, myrowfix, contents[1, ]))
}
