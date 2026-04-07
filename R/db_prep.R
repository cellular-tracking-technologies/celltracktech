db_prep <- function(contents, filetype, sensor,y,begin) {
  timecols <- c("Time") # , "recorded at", "gps at", "RecordedAt", "recorded.at", "gps.at")
  for (x in timecols) {
    if (x %in% names(contents)) {
      contents <- dplyr::filter(contents, (!!as.name(x)) < Sys.time() & (!!as.name(x)) > begin)
    }
  }
  contents <- data.frame(contents)
  if (!is.null(contents) & nrow(contents) > 0) {
    contents$station_id <- sensor
    contents$path <- y
    if (filetype == "gps") {
      colnames(contents)[colnames(contents) == "recorded.at"] <- "recorded_at"
      contents$recorded_at <- as.character(contents$recorded_at)
      colnames(contents)[colnames(contents) == "gps.at"] <- "gps_at"
      contents$gps_at <- as.character(contents$gps_at)
      contents <- contents[which(!is.na(contents$latitude)),]
      if ("mean.lat" %in% colnames(contents) | "mean lat" %in% colnames(contents)) {
        colnames(contents)[colnames(contents) %in% c("mean.lat", "mean lat")] <- "mean_lat"
        colnames(contents)[colnames(contents) %in% c("mean.lng", "mean lng")] <- "mean_lng"
        colnames(contents)[colnames(contents) %in% c("n.fixes", "n fixes")] <- "n_fixes"
      } else {
        contents$mean_lat <- NA
        contents$mean_lng <- NA
        contents$n_fixes <- NA
      }
      names(contents) <- sapply(names(contents), function(x) gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", x))
      # if(fix=TRUE) {
      #  query <- querygen(contents[1,])
      #  res <- DBI::dbGetQuery(conn, paste0("select * from gps where ", query))
      #  if(nrow(res) > 0) {
      #    me <- data.frame(matrix(ncol=ncol(contents), nrow=0))
      #    names(me) <- names(contents)
      #    contents <- me
      #  }
      # }
    } else if (filetype == "raw") {
      # print(names(contents))
      contents <- contents[!is.na(contents$TagId), ]
      if (!(any(tolower(names(contents)) == "validated"))) {
        contents$validated <- NA
      }

      contents$RadioId <- as.integer(contents$RadioId)
      contents$TagRSSI <- as.integer(contents$TagRSSI)
      contents$NodeId <- toupper(contents$NodeId)
      names(contents) <- sapply(names(contents), function(x) gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", x))
      names(contents) <- tolower(names(contents))
      # if(is.na(sensor)) {
      #  mmy <- paste0("select * from raw where time between '", min(contents$time),"' and '", max(contents$time), "'")
      #  sametime <- dbGetQuery(conn, mmy)
      #  sametime$id <- NULL
      #  contents <- rbind(sametime, contents)
      # }

      if (length(which(!is.na(contents$node_id))) > 0) { # if there is anything beside NA nodes
        nodecheck <- contents[!is.na(contents$node_id), ]
        nodecheck <- nodecheck[!duplicated(nodecheck[c("time", "tag_id", "node_id", "tag_rssi")]), ]
        badrec <- nodecheck[duplicated(nodecheck[c("time", "tag_id", "node_id")]), ]

        if (nrow(badrec) > 0) {
          nodecheck$id <- paste(nodecheck$time, nodecheck$tag_id, nodecheck$node_id)
          badrec$id <- paste(badrec$time, badrec$tag_id, badrec$node_id)
          nodecheck <- nodecheck[!nodecheck$id %in% badrec$id, ]
          nodecheck$id <- NULL
        }
        # print(nrow(nodecheck))
        contents <- rbind(nodecheck, contents[is.na(contents$node_id), ])

        if (is.na(sensor)) {
          contents <- contents[is.na(contents$station_id), ]
        }
      }

      if (length(which(nchar(contents$tag_id) != 8)) > 0) { # if there are tag ids greater than 8...
        contents <- contents[-which(nchar(contents$tag_id) != 8), ] # drop rows where TagId not 8 characters
      }

      # if(fix=TRUE) {
      #  query <- querygen(contents[1,])
      #  res <- DBI::dbGetQuery(conn, paste0("select * from raw where ", query))
      #  if(nrow(res) > 0) {
      #    me <- data.frame(matrix(ncol=ncol(contents), nrow=0))
      #    names(me) <- names(contents)
      #    contents <- me
      #  }
      # }
    } else if (filetype == "node_health") {
      contents$Battery[which(contents$Battery > 9)] <- NA
      if (ncol(contents) < 9) {
        contents$RecordedAt <- NA
        contents$Firmware <- NA
        contents$SolarVolts <- NA
        contents$SolarCurrent <- NA
        contents$CumulativeSolarCurrent <- NA
        contents$Latitude <- NA
        contents$Longitude <- NA

      } else if(ncol(contents) > 9) {
        contents <- contents[which(contents$Latitude < 90 & contents$Latitude > -90),]
        #contents <- contents[contents$CumulativeSolarCurrent < 2147483647,]
      }
      nodeids <- toupper(unique(contents$NodeId))
      names(contents) <- sapply(names(contents), function(x) gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", x))
      names(contents) <- tolower(names(contents))

      contents <- contents[!duplicated(contents[c("time", "node_id", "recorded_at")]), ]
      # if(fix=TRUE) {
      #  query <- querygen(contents[1,])
      #  res <- DBI::dbGetQuery(conn, paste0("select * from node_health where ", query))
      #  if(nrow(res) > 0) {
      #    me <- data.frame(matrix(ncol=ncol(contents), nrow=0))
      #    names(me) <- names(contents)
      #    contents <- me
      # }
      # }

      # ONLY SUPPORTS V2 NODES FOR NOW FOR DB
      # contents <- contents[,1:13]
    } else if (filetype == "blu") {
      # print(names(contents))
      contents <- contents[which(!is.na(contents$TagId)), ]
      contents$RadioId <- as.integer(contents$RadioId)
      contents$TagRSSI <- as.integer(contents$TagRSSI)
      contents$UsbPort <- as.integer(contents$UsbPort)
      contents$BluRadioId <- as.integer(contents$BluRadioId)
      contents$Sync <- as.integer(contents$Sync)
      contents$Product <- as.integer(contents$Product)
      contents$Revision <- as.integer(contents$Revision)
      contents$Payload <- as.character(contents$Payload)
      # contents$BatteryVoltage <- as.integer(parseint(contents$Payload)[[1]])
      # contents$Temperature <- as.integer(parseint(contents$Payload)[[2]])

      names(contents) <- sapply(names(contents), function(x) gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", x))
      names(contents) <- tolower(names(contents))
      # if(is.na(sensor)) {
      #  mmy <- paste0("select * from raw where time between '", min(contents$time),"' and '", max(contents$time), "'")
      #  sametime <- dbGetQuery(conn, mmy)
      #  sametime$id <- NULL
      #  contents <- rbind(sametime, contents)
      # }

      if (length(which(!is.na(contents$node_id))) > 0) { # if there is anything beside NA nodes
        nodecheck <- contents[which(!is.na(contents$node_id)), ]
        nodecheck <- nodecheck[!duplicated(nodecheck[c("time", "tag_id", "node_id", "tag_rssi")]), ]
        badrec <- nodecheck[duplicated(nodecheck[c("time", "tag_id", "node_id")]), ]
        if (nrow(badrec) > 0) {
          nodecheck$id <- paste(nodecheck$time, nodecheck$tag_id, nodecheck$node_id)
          badrec$id <- paste(badrec$time, badrec$tag_id, badrec$node_id)
          #nodecheck <- nodecheck[!nodecheck$id %in% badrec$id, ]
          nodecheck$id <- NULL
        }
        # print(nrow(nodecheck))
        contents <- rbind(nodecheck, contents[is.na(contents$node_id), ])

        if (is.na(sensor)) {
          contents <- contents[is.na(contents$station_id), ] #this is for bringing in raw node files
        }
      }

      if (length(which(nchar(contents$tag_id) != 8)) > 0) { # if there are tag ids greater than 8...
        contents <- contents[-which(nchar(contents$tag_id) != 8), ] # drop rows where TagId not 8 characters
      }

      # if(fix=TRUE) {
      #  query <- querygen(contents[1,])
      #  res <- DBI::dbGetQuery(conn, paste0("select * from raw where ", query))
      #  if(nrow(res) > 0) {
      #    me <- data.frame(matrix(ncol=ncol(contents), nrow=0))
      #    names(me) <- names(contents)
      #    contents <- me
      #  }
      # }
    } else {
      nodeids <- c()
    }
  }
  if (any(row.names(contents) == "NA")) {contents <- contents[-which(row.names(contents) == "NA"), ]}
  return(contents)}
