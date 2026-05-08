get_data <- function(thisproject, outpath, f = NULL, my_station, beginning, ending, filetypes) {
  # print("getting your file list")
  # projbasename <- thisproject$name
  projbasename <- stringr::str_trim(thisproject$name) # trim project names so no trailing spaces (windows does not like spaces)
  id <- thisproject[["id"]]
  myfiles <- list.files(file.path(outpath), recursive = TRUE)
  dir.create(file.path(outpath, projbasename), showWarnings = FALSE)
  files_loc <- sapply(strsplit(myfiles, "/"), tail, n = 1)
  my_stations <- get_stations(project_id = id)
  if (!is.null(my_station)) {
    my_stations[["stations"]] <- list(my_stations[[1]][[which(sapply(my_stations[[1]],
                                                                     function(x) x[["station"]][["id"]] == my_station))]])
  }
  files_avail <- lapply(my_stations[["stations"]], function(station, mybeginning = beginning, myending = ending) {
    message(paste0("processing station: ", station[["station"]][["id"]]))
    if (is.null(mybeginning)) {
      beginning <- as.POSIXct(station[["deploy-at"]], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE)
    } else {
      beginning <- as.POSIXct(as.Date(mybeginning), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE)
    }
    kwargs <- list(
      station_id = station[["station"]][["id"]],
      begin = beginning
    )
    message(is.null(myending))
    if (!is.null(myending)) {
      kwargs[["end"]] <- as.POSIXct(as.Date(myending), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE)
    } else if (!is.null(station[["end-at"]])) {
      kwargs[["end"]] <- as.POSIXct(station[["end-at"]], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE)
    }

    message(paste0("station_id: ", kwargs$station_id, ", begin: ", kwargs$begin))
    message("getting station file list...")
    file_info <- do.call(get_station_file_list_paginated, kwargs)
    outfiles <- file_info[["files"]]

    message(paste0('outfiles: ', length(outfiles), ' files found'))
    return(outfiles)
  })
  message("getting files available for those stations...")
  extract_files <- function(x) {
    if (is.list(x) && !is.null(x[["id"]]) && !is.null(x[["name"]])) {
      return(data.frame(id = x[["id"]], name = x[["name"]], stringsAsFactors = FALSE))
    } else if (is.list(x)) {
      return(do.call(rbind, lapply(x, extract_files)))
    } else {
      return(NULL)
    }
  }
  filesdf <- extract_files(files_avail)
  if (is.null(filesdf) || nrow(filesdf) == 0) {
    message("no files found from API")
    return(list())
  }
  filenames <- filesdf$name[grepl("CTT", filesdf$name)]
  message("got the file list; comparing against your files")
  files_to <- filenames[!filenames %in% files_loc]
  message("comparison complete")

  filesdf <- filesdf[filesdf$name %in% files_to, ]
  ids <- filesdf$id
  file_names <- filesdf$name
  message("prepped list of filenames to get")
  if (is.null(filetypes)) {filetypes <- c("raw", "node_health", "gps", "blu")}
  filetypeget <- unlist(sapply(file_names, function(x) get_file_info(x)["filetype"]))
  filesget <- data.frame(ids, file_names, filetypeget)
  filesget <- filesget[filesget$filetypeget %in% filetypes,]
  message(paste("about to download", nrow(filesget), "files"))

  # x = file ids
  # y = file names
  # SOMEWHERE HERE V3 NODES ARE GETTING DELETED
  get_files <- function(x, y) {
    splitfile <- unlist(strsplit(y, "CTT-"))
    fileinfo <- splitfile[2]
    sensorid <- unlist(strsplit(fileinfo, "-"))
    sensor <- sensorid[1]
    filenameinfo <- sensorid[2]
    file_info <- unlist(strsplit(filenameinfo, "\\."))[1]
    filetype <- ifelse(is.na(as.integer(file_info)), file_info, "sensorgnome")
    message(filetype)
    if (is.na(filetype)) {
      filetype <- "none"
    } else if (filetype == "node") {
      filetype <- "node_health"
    } else if (filetype == "data") {
      filetype <- "raw"
    }
    if (filetype %in% filetypes) {
      faul <- which(sapply(my_stations[["stations"]], function(sta) sta$station$id == sensor))
      if (length(faul) > 1) {
        begin <- sapply(faul, function(x) as.POSIXct(my_stations[["stations"]][[x]]$`deploy-at`, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE))
        begin <- max(begin)
      } else {
        begin <- as.POSIXct(my_stations[["stations"]][[faul]]$`deploy-at`, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE)
      }

      contents <- download_files(file_id = x)
      if (filetype == "raw") {
        contents <- httr::content(contents, type = "text", col_types = list(NodeId = "c"))
      } else if(filetype == "blu") {
        contents <- httr::content(contents)
      } else {
        contents <- httr::content(contents, type = "text")
      }
      if (!is.null(contents)) { #& filetype %in% c("raw", "node_health", "gps", "ble", "blu")) {
        dir.create(file.path(outpath, projbasename, sensor), showWarnings = FALSE)
        dir.create(file.path(outpath, projbasename, sensor, filetype), showWarnings = FALSE)
        message(paste("downloading",y,"to",file.path(outpath, projbasename, sensor, filetype)))
        message(x)
        if(is.character(contents)) {write(contents, file = gzfile(file.path(outpath, projbasename, sensor, filetype, y)))
        } else {
          write.csv(contents, file = gzfile(file.path(outpath, projbasename, sensor, filetype, y)), row.names = F)
        }
        e <- file.path(outpath, projbasename, sensor, filetype, y)
        if (!is.null(f)) {
          if(filetype %in% c("raw", "node_health", "gps", "blu")) {
            contents <- file_handle(e, filetype)[[1]]
            message(begin)
            contents <- db_prep(contents, filetype, sensor, y, begin)
            # z <- db_insert(contents, filetype, f, y)
            z <- db_insert(contents=contents, filetype=filetype, conn=f, y=y)

          }
        }
      }
    }
    if (!exists("z")) {
      z <- NULL
    }
    return(z)
  }

  failed <- Map(get_files, filesget$ids, filesget$file_names)
  message("done getting files")
  return(failed)
}
