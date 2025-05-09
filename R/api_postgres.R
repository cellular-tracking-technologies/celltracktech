# source("~/Documents/celltracktech/R/newdb.R")
Correct_Colnames <- function(df) {
  rowval <- gsub("^X\\.", "-", colnames(df))
  rowval <- gsub("^X", "", rowval)
  DatePattern <- "^[[:digit:]]{4}\\.[[:digit:]]{2}\\.[[:digit:]]{2}[T,\\.][[:digit:]]{2}\\.[[:digit:]]{2}\\.[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?"
  rowval[which(grepl(DatePattern, rowval))] <- as.character(as.POSIXct(rowval[grepl(DatePattern, rowval)], format = "%Y.%m.%d.%H.%M.%S", tz = "UTC"))
  return(rowval)
}

DatePattern <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[T, ][[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?"
is.POSIXct <- function(x) inherits(x, "POSIXct")

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

host <- "https://api.internetofwildlife.com/"
# host <- "http://10.1.10.252:8033"
project <- "/station/api/projects"
stations <- "/station/api/stations/"
files <- "/station/api/file-list"
file_types <- c("data", "node-data", "gps", "log", "telemetry", "sensorgnome", "ble", "blu")

project_list <- function(my_token, myproject = NULL) {
  projects <- httr::content(httr::POST(host, path = project, body = list(token = my_token), encode = "json"))
  print(paste('projects', projects))
  projects <- projects[["projects"]]
  print(projects)
  if (!is.null(myproject)) {

    projects <- tryCatch({
      print(paste('The project name you entered is:', myproject))

      list(projects[[which(sapply(projects, function(x) x[["name"]]) == myproject)]])
    }, error = function(err) {
      print(paste('Error:', conditionMessage(err)))
      cat('The project you entered is not found in your project list. Check your spelling and if you have access to the project.\n')
    })
    # projects <- list(projects[[which(sapply(projects, function(x) x[["name"]]) == myproject)]])
    print(projects)
  }
  return(projects)
}

pop_proj <- function(a, conn) {
  b <- unname(as.data.frame(a))
  vars <- paste(DBI::dbListFields(conn, "ctt_project"), sep = "", collapse = ",")
  insertnew <- DBI::dbSendQuery(conn, paste("INSERT INTO ", "ctt_project", " (", vars, ") VALUES ($1, $2) ON CONFLICT DO NOTHING", sep = ""))
  # it is possible you should be using dbSendStatement for all of these
  DBI::dbBind(insertnew, params = b)
  DBI::dbClearResult(insertnew)

  basename <- a$name
  id <- a[["id"]]
  my_stations <- getStations(project_id = id)
  print("RETURNED FROM API")
  print(my_stations)
  mystations <- lapply(my_stations$stations, function(c) {
    c <- as.data.frame(t(unlist(c)), stringsAsFactors = FALSE)

    c$project_id <- id
    colnames(c)[colnames(c) == "station.db-id"] <- "db_id"
    colnames(c)[colnames(c) == "station.id"] <- "station_id"
    colnames(c)[colnames(c) == "deploy-at"] <- "deploy_at"
    if (is.null(c$`end-at`)) {
      c$end_at <- NA
    } else {
      colnames(c)[colnames(c) == "end-at"] <- "end_at"
    }
    return(c)
  })
  mystations <- as.data.frame(dplyr::bind_rows(mystations))
  MYSTATIONS <- list(unique(mystations$station_id))
  mystations <- unname(mystations)
  print("FORMATTED")
  print(mystations)

  # insertnew <- DBI::dbSendQuery(conn, paste("INSERT INTO ","station (station_id)"," VALUES ($1)
  #                                     ON CONFLICT DO NOTHING",sep=""))
  # dbBind(insertnew, params=MYSTATIONS)
  # dbClearResult(insertnew)

  vars <- paste(DBI::dbListFields(conn, "ctt_project_station"), sep = "", collapse = ",")
  # print(vars)
  insertnew <- DBI::dbSendQuery(conn, paste("INSERT INTO ", "ctt_project_station", " (", vars, ") VALUES ($1, $4, $2, $3, $5)
                                       ON CONFLICT DO NOTHING", sep = ""))
  DBI::dbBind(insertnew, params = mystations)
  DBI::dbClearResult(insertnew)
}

post <- function(endpoint, payload = NULL) {
  payload_to_send <- list(token = my_token)
  if (!is.null(payload)) {
    payload_to_send <- c(payload_to_send, payload)
  }
  print('payload to send')
  print(payload_to_send)
  response <- httr::POST(host, path = endpoint, body = payload_to_send, encode = "json", httr::timeout(3000))
  httr::stop_for_status(response)
  return(response)
}

getStations <- function(project_id) {
  print('getStations payload')
  print(project_id)
  out <- post(endpoint = stations, payload = list("project-id" = project_id))
  return(httr::content(out))
}

getStationFileList <- function(station_id, begin, filetypes = NULL, end = NULL) {
  endpoint <- files
  payload <- list("station-id" = station_id, begin = as.Date(begin))
  print(paste('getStationFileList payload', payload))
  print(paste('getStationFileList endpoint', endpoint))

  if (!is.null(filetypes)) {
    add_types <- filetypes[filetypes %in% file_types]
    if (length(which(!filetypes %in% file_types)) > 0) {
      print(paste("WARNING: invalid file type specified - ignoring:", filetypes[!filetypes %in% file_types]))
    }
    payload[["file-types"]] <- add_types
  }
  if (!is.null(end)) {
    payload[["end"]] <- as.Date(end)
  }
  return(httr::content(post(endpoint = endpoint, payload = payload)))
}

downloadFiles <- function(file_id) {
  endpoint <- "/station/api/download-file/"
  payload <- list("file-id" = file_id)
  print(paste('downloadFiles payload', payload))

  response <- tryCatch(
    {
      post(endpoint = endpoint, payload = payload)
    },
    error = function(cond) {
      message("Here's the original error message:")
      message(conditionMessage(cond))
      payload <- c(payload, "bypass-encoding" = "plain")
      # Choose a return value in case of error
      post(endpoint = endpoint, payload = payload)
    }
  )
  return(response)
}

#' Create Postgres database
#'
#' This function allows you to create a blank version of the CTT designed database
#' @param conn the connection to your local database
#' @export
#' @examples
#' create_db(conn)
create_db <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS ctt_project
  (
    id	smallint PRIMARY KEY,
    name	TEXT NOT NULL UNIQUE
  )")
  #

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS nodes
  (
    node_id TEXT NOT NULL PRIMARY KEY
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS data_file
  (
    path TEXT PRIMARY KEY
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS ctt_project_station
  (
    db_id	smallint PRIMARY KEY,
    project_id smallint NOT NULL,
    station_id	TEXT NOT NULL,
    deploy_at	TIMESTAMP with time zone,
    end_at	TIMESTAMP with time zone,
    FOREIGN KEY (project_id)
      REFERENCES ctt_project (id)
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS raw
  (
    id	SERIAL PRIMARY KEY,
    path  TEXT NOT NULL,
    radio_id smallint NOT NULL,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    validated smallint,
    time TIMESTAMP with time zone NOT NULL,
    station_id TEXT
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS blu
  (
    id	SERIAL PRIMARY KEY,
    path  TEXT NOT NULL,
    radio_id smallint,
    usb_port smallint,
    blu_radio_id smallint,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    sync integer,
    product smallint,
    revision smallint,
    payload text,
    time TIMESTAMP with time zone NOT NULL,
    station_id TEXT
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS node_health
  (
    PRIMARY KEY (radio_id, node_id, time, station_id),
    time TIMESTAMP with time zone NOT NULL,
    radio_id smallint,
    node_id TEXT,
    node_rssi smallint,
    battery NUMERIC(3,2),
    celsius smallint,
    recorded_at TIMESTAMP with time zone,
    firmware TEXT,
    solar_volts NUMERIC(4,2),
    solar_current smallint,
    cumulative_solar_current integer,
    latitude NUMERIC(8,6),
    longitude NUMERIC(9,6),
    station_id TEXT,
    path  TEXT NOT NULL,
    FOREIGN KEY (node_id)
      REFERENCES nodes (node_id)
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS gps
  (
    path  TEXT NOT NULL,
    latitude NUMERIC(8,6),
    longitude NUMERIC(9,6),
    altitude NUMERIC(6,1),
    quality smallint,
    gps_at TIMESTAMP with time zone,
    recorded_at TIMESTAMP with time zone,
    station_id TEXT,
    mean_lat NUMERIC(8,6),
    mean_lng NUMERIC(9,6),
    n_fixes smallint,
    PRIMARY KEY (gps_at, station_id)
  )")

  DBI::dbExecute(conn, "
  CREATE TABLE IF NOT EXISTS node_raw
  (
    id SERIAL PRIMARY KEY,
    path  TEXT NOT NULL,
    radio_id TEXT,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    time TIMESTAMP with time zone NOT NULL,
    validated smallint,
    station_id TEXT
  )")

  DBI::dbExecute(conn, "
  CREATE TABLE IF NOT EXISTS node_blu
  (
    id SERIAL PRIMARY KEY,
    path  TEXT NOT NULL,
    radio_id smallint,
    usb_port smallint,
    blu_radio_id smallint,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    sync integer,
    product smallint,
    revision smallint,
    payload text,
    time TIMESTAMP with time zone NOT NULL,
    station_id TEXT
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS node_health_from_node
  (
    PRIMARY KEY (node_id, time, station_id),
    time TIMESTAMP with time zone NOT NULL,
    up_time BIGINT,
    power_ok smallint,
    batt_mv smallint,
    batt_temp_c smallint,
    charge_mv smallint,
    charge_ma smallint,
    charge_temp_c smallint,
    node_temp_c smallint,
    energy_used_mah smallint,
    sd_free smallint,
    sub_ghz_det smallint,
    ble_det smallint,
    errors TEXT,
    node_id TEXT,
    station_id TEXT,
    path  TEXT NOT NULL,
    FOREIGN KEY (node_id)
      REFERENCES nodes (node_id)
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS node_gps
  (
    path  TEXT NOT NULL,
    latitude NUMERIC(8,6),
    longitude NUMERIC(9,6),
    altitude NUMERIC(6,1),
    gps_at TIMESTAMP WITH TIME ZONE,
    hdop NUMERIC(6,2),
    vdop NUMERIC(6,2),
    pdop NUMERIC(6,2),
    navigation_mode smallint,
    satellites NUMERIC(5,2),
    on_time NUMERIC(3,0),
    station_id TEXT,
    node_id TEXT,
    PRIMARY KEY (gps_at, node_id)
  )")
}

create_duck <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS ctt_project
  (
    id	smallint PRIMARY KEY,
    name	TEXT NOT NULL UNIQUE
  )")
  #

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS nodes
  (
    node_id TEXT NOT NULL PRIMARY KEY
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS data_file
  (
    path TEXT PRIMARY KEY
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS ctt_project_station
  (
    db_id	smallint PRIMARY KEY,
    project_id smallint NOT NULL,
    station_id	TEXT NOT NULL,
    deploy_at	TIMESTAMP with time zone,
    end_at	TIMESTAMP with time zone,
    FOREIGN KEY (project_id)
      REFERENCES ctt_project (id)
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")

  DBI::dbExecute(conn, "
  CREATE SEQUENCE IF NOT EXISTS seq_id START 1;
  CREATE TABLE IF NOT EXISTS raw
  (
    id integer primary key default nextval('seq_id'),
    path  TEXT NOT NULL,
    radio_id smallint NOT NULL,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    validated smallint,
    time TIMESTAMP with time zone NOT NULL,
    station_id TEXT
  )")

  DBI::dbExecute(conn, "
  CREATE SEQUENCE IF NOT EXISTS seq_idb START 1;
  CREATE TABLE IF NOT EXISTS blu
  (
    id integer primary key default nextval('seq_idb'),
    path  TEXT NOT NULL,
    radio_id smallint,
    usb_port smallint,
    blu_radio_id smallint,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    sync integer,
    product smallint,
    revision smallint,
    payload text,
    time TIMESTAMP with time zone NOT NULL,
    station_id TEXT
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS node_health
  (
    PRIMARY KEY (radio_id, node_id, time, station_id),
    time TIMESTAMP with time zone NOT NULL,
    radio_id smallint,
    node_id TEXT,
    node_rssi smallint,
    battery NUMERIC(3,2),
    celsius smallint,
    recorded_at TIMESTAMP with time zone,
    firmware TEXT,
    solar_volts NUMERIC(4,2),
    solar_current smallint,
    cumulative_solar_current integer,
    latitude NUMERIC(8,6),
    longitude NUMERIC(9,6),
    station_id TEXT,
    path  TEXT NOT NULL,
    FOREIGN KEY (node_id)
      REFERENCES nodes (node_id)
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS gps
  (
    path  TEXT NOT NULL,
    latitude NUMERIC(8,6),
    longitude NUMERIC(9,6),
    altitude NUMERIC(6,1),
    quality smallint,
    gps_at TIMESTAMP with time zone,
    recorded_at TIMESTAMP with time zone,
    station_id TEXT,
    mean_lat NUMERIC(8,6),
    mean_lng NUMERIC(9,6),
    n_fixes smallint,
    PRIMARY KEY (gps_at, station_id)
  )")

  DBI::dbExecute(conn, "
  CREATE SEQUENCE IF NOT EXISTS seq_id START 1;
  CREATE TABLE IF NOT EXISTS node_raw
  (
    id integer primary key default nextval('seq_id'),
    path  TEXT NOT NULL,
    radio_id TEXT,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    time TIMESTAMP with time zone NOT NULL,
    validated smallint,
    station_id TEXT
  )")

  DBI::dbExecute(conn, "
  CREATE SEQUENCE IF NOT EXISTS seq_idb START 1;
  CREATE TABLE IF NOT EXISTS node_blu
  (
    id integer primary key default nextval('seq_idb'),
    path  TEXT NOT NULL,
    radio_id smallint,
    usb_port smallint,
    blu_radio_id smallint,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    sync integer,
    product smallint,
    revision smallint,
    payload text,
    time TIMESTAMP with time zone NOT NULL,
    station_id TEXT
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS node_health_from_node
  (
    PRIMARY KEY (node_id, time, station_id),
    time TIMESTAMP with time zone NOT NULL,
    up_time BIGINT,
    power_ok smallint,
    batt_mv smallint,
    batt_temp_c smallint,
    charge_mv smallint,
    charge_ma smallint,
    charge_temp_c smallint,
    node_temp_c smallint,
    energy_used_mah smallint,
    sd_free smallint,
    sub_ghz_det smallint,
    ble_det smallint,
    errors TEXT,
    node_id TEXT,
    station_id TEXT,
    path  TEXT NOT NULL,
    FOREIGN KEY (node_id)
      REFERENCES nodes (node_id)
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS node_gps
  (
    path  TEXT NOT NULL,
    latitude NUMERIC(8,6),
    longitude NUMERIC(9,6),
    altitude NUMERIC(6,1),
    gps_at TIMESTAMP WITH TIME ZONE,
    hdop smallint,
    vdop smallint,
    pdop smallint,
    navigation_mode smallint,
    satellites NUMERIC(5,2),
    on_time NUMERIC(3,0),
    station_id TEXT,
    node_id TEXT,
    PRIMARY KEY (gps_at, node_id)
  )")
}

querygen <- function(mycont) {
  pieces <- paste(names(mycont), mycont, sep = " = ")
  na <- grep(" = NA", pieces)
  if (length(na > 0)) {
    pieces[na] <- gsub("= NA", "is null", pieces[na])
  }
  pieces <- paste(pieces, collapse = " and ")
  return(pieces)
}

timeset <- function(g) {
  unname(sapply(g, function(h) ifelse(is.na(h), NA, paste(as.character(h), "UTC"))))
}

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

db_insert <- function(contents, filetype, conn, sensor=NA, y, begin=NULL) {
  if(any(colnames(contents) == "node_id")) {
    contents$node_id <- toupper(contents$node_id)
    if (length(which(!is.na(contents$node_id))) > 0) {
      nodeids <- contents$node_id[which(!is.na(contents$node_id))]
      insertnew <- DBI::dbSendQuery(conn,
                                    paste("INSERT INTO ",
                                    "nodes (node_id)",
                                    " VALUES ($1)
                                    ON CONFLICT DO NOTHING", sep = ""))

      DBI::dbBind(insertnew, params = list(unique(nodeids)))
      DBI::dbClearResult(insertnew)
    }
  } else {
    nodeids <- c()
  }
  if (filetype %in% c("raw",
                      "node_health",
                      "gps",
                      "blu",
                      'node_raw',
                      'node_health_from_node',
                      'node_gps',
                      'node_blu') & nrow(contents) > 0) {
    if (filetype %in% c("raw", "blu", 'node_raw', 'node_blu')) {
      vars <- paste(DBI::dbListFields(conn, filetype)[2:length(DBI::dbListFields(conn, filetype))],
                    sep = "",
                    collapse = ",")
      vals <- paste(seq_along(1:(length(DBI::dbListFields(conn, filetype)) - 1)),
                    sep = "",
                    collapse = ", $")

      contents <- contents[, DBI::dbListFields(conn, filetype)[2:length(DBI::dbListFields(conn, filetype))]] # need path and station_id columns
      contents
    } else {
      vars <- paste(DBI::dbListFields(conn, filetype), sep = "", collapse = ",")
      vals <- paste(seq_along(1:length(DBI::dbListFields(conn, filetype))),
                    sep = "",
                    collapse = ", $")
      names(contents) <- tolower(names(contents))
      contents <- contents[, DBI::dbListFields(conn, filetype)]
    }

    # browser()
    h <- tryCatch({
      tryCatch({
        DBI::dbWriteTable(conn, filetype, contents, append = TRUE)
        query = paste("INSERT INTO ", "data_file (path)", " VALUES ($1) ON CONFLICT DO NOTHING", sep = "")
        insertnew <-DBI::dbSendQuery(conn, query)
        # insertnew <- DBI::dbSendQuery(conn,
        #                               paste("INSERT INTO ",
        #                                     "data_file (path)",
        #                                     " VALUES ($1) ON CONFLICT DO NOTHING", sep = ""))  #CTT-FC16AD87C466-node-health.2022-07-15_104908.csv.gz
        DBI::dbBind(insertnew, params = list(y))
        DBI::dbClearResult(insertnew)
        return(NULL)
      },
      error = function(err) {
        # error handler picks up where error was generated, in Bob's script it breaks if header is missing
        # myquery <- paste("INSERT INTO ", filetype, " (", vars, ") VALUES ($", vals, ")
        # ON CONFLICT DO NOTHING", sep = "")
        myquery <- paste("INSERT INTO ", filetype, " (", vars, ") VALUES ($", vals, ")", sep = "")
        insertnew <- DBI::dbSendQuery(conn, myquery)
        DBI::dbBind(insertnew, params = unname(contents))
        DBI::dbClearResult(insertnew)
        insertnew <- DBI::dbSendQuery(conn, paste("INSERT INTO ", "data_file (path)", " VALUES ($1)
                                         ON CONFLICT DO NOTHING", sep = ""))  #CTT-FC16AD87C466-node-health.2022-07-15_104908.csv.gz
        DBI::dbBind(insertnew, params = list(y))
        DBI::dbClearResult(insertnew)
      })
    },
    error = function(err) {
      print(paste('h error', err))
      return(list(err, contents, y))
    }
    )
  }
  if (!exists("h")) {
    h <- NULL # h is boolean
  }
  # print(paste('what the hell is h???', h)) # h is boolean
  return(h)
}

get_data <- function(thisproject, outpath, f = NULL, my_station, beginning, ending, filetypes) {
  # print("getting your file list")
  # projbasename <- thisproject$name
  projbasename <- stringr::str_trim(thisproject$name) # trim project names so no trailing spaces (windows does not like spaces)
  id <- thisproject[["id"]]
  myfiles <- list.files(file.path(outpath), recursive = TRUE)
  dir.create(file.path(outpath, projbasename), showWarnings = FALSE)
  files_loc <- sapply(strsplit(myfiles, "/"), tail, n = 1)
  my_stations <- getStations(project_id = id)
  if (!is.null(my_station)) {
    my_stations[["stations"]] <- list(my_stations[[1]][[which(sapply(my_stations[[1]],
                                                                     function(x) x[["station"]][["id"]] == my_station))]])
  }
  files_avail <- lapply(my_stations[["stations"]], function(station, mybeginning = beginning, myending = ending) {
    print(station)
    if (is.null(mybeginning)) {
      beginning <- as.POSIXct(station[["deploy-at"]], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE)
    } else {
      beginning <- as.POSIXct(as.Date(mybeginning), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE)
    }
    kwargs <- list(
      station_id = station[["station"]][["id"]],
      begin = beginning
    )
    print(is.null(myending))
    if (!is.null(myending)) {
      kwargs[["end"]] <- as.POSIXct(as.Date(myending), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE)
    } else if (!is.null(station[["end-at"]])) {
      kwargs[["end"]] <- as.POSIXct(station[["end-at"]], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC", optional = TRUE)
    }

    print(kwargs)
    print("getting station file list...")
    file_info <- do.call(getStationFileList, kwargs)
    outfiles <- file_info[["files"]]

    return(outfiles)
  })
  print("getting files available for those stations...")
  filenames <- unname(rapply(files_avail, grep, pattern = "CTT", value = TRUE))
  print("got the file list; comparing against your files")
  files_to <- filenames[!filenames %in% files_loc]
  print("comparison complete")

  # browser()
  allfiles <- rapply(files_avail, function(z) z %in% files_to, how = "unlist") # this is the super intensive, time consuming function...
  ids <- unlist(files_avail)[which(allfiles) - 1]
  print(paste("about to get", length(ids), "files"))
  file_names <- unlist(files_avail)[which(allfiles)]
  print("prepped list of filenames to get")
  if (is.null(filetypes)) {filetypes <- c("raw", "node_health", "gps", "blu")}
  filetypeget <- unlist(sapply(file_names, function(x) get_file_info(x)["filetype"]))
  filesget <- data.frame(ids, file_names, filetypeget)
  filesget <- filesget[filesget$filetypeget %in% filetypes,]

  # x = file ids
  # y = file names
  get_files <- function(x, y) {
    print(x)
    print(y)
    splitfile <- unlist(strsplit(y, "CTT-"))
    fileinfo <- splitfile[2]
    sensorid <- unlist(strsplit(fileinfo, "-"))
    sensor <- sensorid[1]
    filenameinfo <- sensorid[2]
    file_info <- unlist(strsplit(filenameinfo, "\\."))[1]
    filetype <- ifelse(is.na(as.integer(file_info)), file_info, "sensorgnome")
    print(filetype)
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

      contents <- downloadFiles(file_id = x)
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
        print(paste("downloading",y,"to",file.path(outpath, projbasename, sensor, filetype)))
        print(x)
        if(is.character(contents)) {write(contents, file = gzfile(file.path(outpath, projbasename, sensor, filetype, y)))
        } else {
          write.csv(contents, file = gzfile(file.path(outpath, projbasename, sensor, filetype, y)), row.names = F)
        }
        e <- file.path(outpath, projbasename, sensor, filetype, y)
        if (!is.null(f)) {
          if(filetype %in% c("raw", "node_health", "gps", "blu")) {
            contents <- file_handle(e, filetype)[[1]]
            print(begin)
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
  print("done getting files")
  return(failed)
}

goodrows <- function(rowlen, rowfix, e, correct, DatePattern, filetype) {
  if (length(rowfix) < 2) {
    fixed <- fixrow(rowlen, rowfix, e, correct, DatePattern, filetype)
  } else {
    fixed <- Map(fixrow, rowlen, rowfix, MoreArgs = list(e = e, DatePattern = DatePattern, correct = correct, filetype = filetype))
    fixed <- data.table::rbindlist(fixed, use.names = FALSE)
  }
  return(fixed)
}

badrow <- function(e, contents, filetype) {
  if (filetype == "raw") {
    correct <- ifelse(ncol(contents) > 5, 6, 5)
  } else if (filetype == "node_health") {
    correct <- ifelse(ncol(contents) > 9, ifelse(ncol(contents) > 13, 19, 13), 6)
  } else if (filetype == "gps") {
    indx <- count.fields(e, sep = ",")
    correct <- ifelse(any(indx == 9), 9, 6)
  }

  file_err <- 0
  indx <- count.fields(e, sep = ",")
  indx[which(is.na(indx))] <- correct
  if (any(indx > correct)) {
    rowfix <- which(indx != correct) - 1
    rowlen <- indx[which(indx != correct)]
    # if (filetype == "gps") {
    #  if(correct == 6 & rowlen == 9) {contents <- contents[rowfix,]}
    # } else {
    file_err <- 3 # what if this is more than 1 row?
    contents <- contents[-rowfix, ]
    fixed <- goodrows(rowlen, rowfix, e, correct, DatePattern, filetype)
    names(fixed) <- names(contents)
    contents <- rbind(contents, fixed)
    # }
  } else if (any(indx < correct)) {
    file_err <- 4
    rowfix <- which(indx != correct) - 1
    rowlen <- indx[which(indx != correct)] # what if this is more than 1 row?
    print(contents[rowfix, ])
    if (filetype == "gps" & correct == 9) {
      if (min(rowfix) < 1) {
        rowfix <- which(indx == 9) - 1
        rowlen <- indx[which(indx == correct)]
        contents <- goodrows(rowlen, rowfix, e, correct, DatePattern, filetype)
      }
    }
    if (!is.POSIXct(contents[rowfix, 1][[1]])) { # does this matter about how it's read in too? look for why it sometimes doesn't cast that way
      # print(contents[rowfix,])
      contents <- contents[-rowfix, ]
    }
    # else if(length(rowfix) < 2) {
    # datetest <- tryCatch({
    #  is.POSIXct(contents[rowfix,1]$Time)
    # }, error = function(cond) {
    #  NA
    # })
  } # else {file_err <- 5}
  if (any(indx < correct) & any(indx > correct)) {
    file_err <- 5
  }
  return(list(contents, file_err))
}

timecheck <- function(contents, myrowfix) {
  time <- ifelse(is.POSIXct(contents[, 1][[1]]), as.POSIXct(myrowfix[1], tz = "UTC"), myrowfix[1])
  return(time)
}

file_handle <- function(e, filetype) {
  print(paste("checking file for errors:", e))
  #print(filetype)
  file_err <- 0
  myrowfix <- c()
  ignore <- FALSE
  if(filetype=="raw") {
    contents <- tryCatch(
      {
        readr::read_csv(e, col_names = TRUE, col_types = list(NodeId="c"))
      },
      error = function(err) {
        return(NULL)
      }
    )
  } else {
    contents <- tryCatch(
      {
        readr::read_csv(e, col_names = TRUE)
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
            myrowfix <- Correct_Colnames(contents)
            myrowfix[1] <- strsplit(Correct_Colnames(contents)[1], "[.]")[[1]][1]
            myrowfix[2] <- strsplit(Correct_Colnames(contents)[2], "[.]")[[1]][1]
            myrowfix[3] <- strsplit(Correct_Colnames(contents)[3], "\\.\\.")[[1]][1] # were there files where this wasn't correctly split?
            myrowfix[3] <- ifelse(myrowfix[3]=="", NA, myrowfix[3])
            myrowfix[4] <- strsplit(Correct_Colnames(contents)[4], "\\.\\.")[[1]][1]
            myrowfix[5] <- strsplit(Correct_Colnames(contents)[5], "\\.\\.")[[1]][1]
            if (nchar(myrowfix[5]) < 1) {
              myrowfix[5] <- NA
            }
            if (length(myrowfix) > 5) {
              myrowfix[6] <- strsplit(Correct_Colnames(contents)[6], "[.]")[[1]][1]
            }
            if (length(myrowfix) > 6) {
              myrowfix[7] <- strsplit(Correct_Colnames(contents)[7], "\\.\\.")[[1]][1]
              myrowfix[7] <- strsplit(myrowfix[7], "[_]")[[1]][1]
              myrowfix[8] <- strsplit(Correct_Colnames(contents)[8], "\\.\\.")[[1]][1]
            }
            if (length(myrowfix) > 9) {
              myrowfix[10] <- strsplit(Correct_Colnames(contents)[10], "\\.\\.")[[1]][1]
              myrowfix[10] <- ifelse(myrowfix[10]=="", NA, myrowfix[10])
              myrowfix[12] <- strsplit(Correct_Colnames(contents)[12], "\\.\\.")[[1]][1]
              myrowfix[13] <- strsplit(Correct_Colnames(contents)[13], "\\.\\.")[[1]][1]
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
        contents <- contents[(nchar(contents$NodeId) == 6 | is.na(contents$NodeId)),]
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

#' Download data
#'
#' This function allows you to download your sensor station data, with the option of simultaneously importing to a local database
#' @param my_token your API key
#' @param outpath where your files are to be downloaded
#' @param db_name (optional) the connection to your local database
#' @param myproject the name of your project on our system
#' @param mystation (optional) the station ID you'd like to download data from
#' @param begin (optional) limit your data download to a start time
#' @param end (optional) limit your data download to an end time
#' @export
#' @examples
#' get_my_data(token, "~/mydata", myproject = "Project Name from CTT Account")
get_my_data <- function(my_token,
                        outpath,
                        db_name = NULL,
                        myproject = NULL,
                        mystation = NULL,
                        begin = NULL,
                        end = NULL,
                        filetypes=NULL) {

  projects <- project_list(my_token, myproject)

  if (!is.null(db_name) & length(grep("postgresql", format(db_name))) > 0) {
    create_db(db_name) # EDIT TO TAKE NEW create_db() when you switch back!
    sapply(projects, pop_proj, conn = db_name)
    failed <- lapply(projects, get_data, f = db_name, outpath = outpath, my_station = mystation, beginning = begin, ending = end, filetypes=filetypes)
  } else if(!is.null(db_name) & length(grep("duckdb", format(db_name))) > 0) {
    create_duck(db_name)
    sapply(projects, pop_proj, conn = db_name)
    failed <- lapply(projects, get_data, f = db_name, outpath = outpath, my_station = mystation, beginning = begin, ending = end, filetypes=filetypes)
  } else {
    failed <- lapply(projects, get_data, outpath = outpath, my_station = mystation, beginning = begin, ending = end, filetypes=filetypes)
  }
  print(paste('failed', failed[[1]]))
  faul <- which(!sapply(failed[[1]], is.null))
  if (length(faul > 0)) {
    failed <- Map(`[`, failed, faul)
    #save(failed, file = file.path(outpath, "caught.RData"))
  } else {
    failed <- "all good!"
    #save(failed, file = file.path(outpath, "caught.RData"))
  }
}

pop <- function(x) { # this was a function written before the data file table was added, no one should need this
  allnode <- DBI::dbReadTable(x, "node_health")
  allgps <- DBI::dbReadTable(x, "gps")
  allbeep <- DBI::dbReadTable(x, "raw")
  insertnew <- DBI::dbSendQuery(conn, paste("INSERT OR IGNORE INTO ", "data_file (path)", " VALUES ($)", sep = ""))
  DBI::dbBind(insertnew, params = list(unique(c(allnode$path, allgps$path, allbeep$path))))
  DBI::dbClearResult(insertnew)

  insertnew <- DBI::dbSendQuery(conn, paste("INSERT OR IGNORE INTO ", "nodes (node_id)", " VALUES ($)", sep = ""))
  DBI::dbBind(insertnew, params = list(unique(allnode$node_id)))
  DBI::dbClearResult(insertnew)
}

#' Insert data into your local database
#'
#' This function allows you to import data you've downloaded to your database
#' @param d the connection to your local database
#' @param outpath where your files were downloaded
#' @param myproject the name of your project on our system
#' @param fix (optional) if TRUE, it will re-check your files to see if any imports were missed
#' @export
#' @examples
#' update_db(conn, "~/mydata", myproject = "Project Name from CTT Account", fix = FALSE)
update_db <- function(d, outpath, myproject, fix = FALSE) {

  ### NEED TO ADD NODE COLUMN CONDITIONAL
  if ('CumulativeSolarCurrent' %in% colnames(df)) {
    df <- df %>%
      rename(radio_id = 'RadioId',
             node_id = 'NodeId',
             node_rssi = 'NodeRSSI',
             battery = 'Battery',
             celsius = 'Celsius',
             recorded_at = 'RecordedAt',
             firmware = 'Firmware',
             solar_volts = 'SolarVolts',
             solar_current = 'SolarCurrent',
             cumulative_solar_current = 'CumulativeSolarCurrent',
             latitude = 'Latitude',
             longitude = 'Longitude',
             up_time = 'UpTime',
             charge_ma = 'AverageChargerCurrentMa',
             energy_used_mah = 'EnergyUsed',
             sd_free = 'SdFree',
             sub_ghz_det = 'Detections',
             errors = 'Errors')
  }

  myfiles <- list.files(file.path(outpath, myproject), recursive = TRUE, full.names = TRUE)
  files_loc <- basename(myfiles)
  allnode <- DBI::dbReadTable(d, "data_file")
  if (fix) {
    res <- DBI::dbGetQuery(d, "select distinct path from gps")
    res2 <- DBI::dbGetQuery(d, "select distinct path from raw")
    res1 <- DBI::dbGetQuery(d, "select distinct path from node_health")
    filesdone <- c(res$path, res1$path, res2$path)
  } else {
    filesdone <- allnode$path
  }
  files_import <- myfiles[which(!files_loc %in% filesdone)]
  files_import <- files_import[unname(sapply(files_import,
                                             function(x) get_file_info(x)[[1]])) %in% c("gps",
                                                                                        "node_health",
                                                                                        "raw",
                                                                                        "blu")]
  #files_import <- files_import[unname(sapply(files_import, function(x) get_file_info(x)[[2]])) == "FC16AD87C466"][1:10]
  write.csv(files_import, file.path(outpath, "files.csv"))
  failed2 <- lapply(files_import, get_files_import, conn = d, outpath=outpath) # outpath=outpath, myproject=myproject)
  # faul <- which(!sapply(failed2[[1]], is.null))
  # if(length(faul) > 0) {
  # failed2 <- Map(`[`, failed2, faul)
  # resave(failed2, file=file.path(outpath, "caught.RData"))
  # } else {
  #  failed2 <- "all good!"
  #  resave(failed2, file=file.path(outpath, "caught.RData"))
  #  }
}

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

get_files_import <- function(e, errtpe = 0, conn, fix = F, outpath=outpath) {
  # e <- file.path(outpath, myproject, e)
  out <- get_file_info(e)
  filetype <- out$filetype

  if (filetype %in% c("raw", "node_health", "gps", "blu")) {
    sensor <- out$sensor
    y <- out$y
    i <- DBI::dbReadTable(conn, "ctt_project_station")
    begin <- i[i$station_id == sensor, ]$deploy_at
    if (length(begin) == 0) {
      begin <- as.POSIXct("2018-01-01")
    }
    # print("attempting import")
    outtest <- file_handle(e, filetype)
    contents <- outtest[[1]]
    errtype <- outtest[[2]]
    # file_err <- fileimp[[2]]
    # print("inserting contents")
    #print(fix)
    print(errtype)
    #print(filetype)
    print(y)
    contents <- db_prep(contents, filetype, sensor, y, begin)
    if(nrow(contents) < 1) {errtype <- 7}
    if (errtype < 7 & errtype != 2) {
      # z <- db_insert(contents, filetype, conn, y)
      z <- db_insert(contents=contents, filetype=filetype, conn=conn, y=y, begin=begin)
      print(paste('get files import z', z))
    } else if(errtype == 7) {
      dir.create(file.path(outpath, "ignore_files"), showWarnings = FALSE)
      file.copy(e, file.path(outpath, "ignore_files"))
      file.remove(e)
    } else {
      dir.create(file.path(outpath, "error_files"), showWarnings = FALSE)
      file.copy(e, file.path(outpath, "error_files"))
      file.remove(e)
    }
  }
  if (!exists("z")) {
    z <- NULL
  }
}

#' Fix database entries
#'
#' This function allows you to copy files with errors or corrupt lines into a new destination to look closer
#' @param d database connection
#' @param outpath where your files were downloaded
#' @param myproject the name of your project at CTT
#' @param dirout where the problem files will be copied
#' @export
#' @examples
#' patch(dbConnect(RPostgres::Postgres(), dbname = db_name), "~/mydata", "My Project", "~/errfiles")
patch <- function(d, outpath, myproject, dirout) {
  # myfiles <- list.files(file.path(outpath, myproject), recursive = TRUE, full.names=TRUE)
  errors <- error_files(file.path(outpath, myproject), dirout, d)
  # myfiles <- names(errors)
  # files_loc <- sapply(strsplit(myfiles, "/"), tail, n=1)
  DBI::dbExecute(d, "UPDATE raw SET node_id=upper(node_id)")
  DBI::dbExecute(d, "UPDATE raw SET tag_id=upper(tag_id)")
  try(DBI::dbSendQuery(conn, "WITH ordered AS (SELECT upper(node_id),
    RANK() OVER (PARTITION BY upper(node_id)) AS rnk
  FROM nodes where node_id is not null
),
to_delete AS (
  SELECT *
  FROM   ordered
  WHERE  rnk > 1
)
delete from nodes using to_delete where nodes.node_id = to_delete.node_id"))
# failed2 <- Map(get_files_import, names(errors), unname(errors), MoreArgs=list(conn=d, fix=T))
}

# x <- data.frame("2021-10-26 18:29:52", 1, "52345578", -91, NA, 1)
# names(x) <- c("Time", "RadioId", "TagId", "TagRSSI", "NodeId", "Validated")
# rbind(contents, x)

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
    print(e)
    fileinfo <- get_file_info(e)
    if (fileinfo$filetype %in% c("raw", "node_health", "gps")) {
      testerr <- file_handle(e, fileinfo$filetype)
      cat(c(testerr[[3]], e), file = fileConn, append = T)
      cat("\n", file = fileConn, append = TRUE)
      testerr <- testerr[[2]]
      if (!is.null(conn)) {
        if (testerr > 0) {
          print("deleting")
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

#' Create database from Node Data files from SD card
#'
#' @param outpath where your files are to be downloaded
#' @param myproject the name of your project on our system
#' @param db_name the connection to your local database
#'
#' @returns database
#' @export
#'
#' @examples
#' create_database(outpath = './data/', myproject = 'Meadows V2', db_name = conn)
create_database = function(
    my_token,
    outpath,
    myproject,
    db_name) {

  # if (is.null(my_token)) {
  #   myList = list()
  #   myList$id = as.integer(1)
  #   myList$name = myproject
  #   projects = list(myList)
  # } else {
  #   projects <- project_list(my_token, myproject)
  # }

  projects <- project_list(my_token, myproject)

  # Checking if database is postgres, duckdb, or remote
  if (!is.null(db_name) && length(grep("postgresql", format(db_name))) > 0) {
    create_db(db_name) # EDIT TO TAKE NEW create_db() when you switch back!
    sapply(projects, pop_proj, conn = db_name)
    # failed <- import_node_data(conn,
    #                            outpath,
    #                            myproject=myproject)

  } else if (!is.null(db_name) && length(grep("duckdb", format(db_name))) > 0) {
    create_duck(db_name)
    sapply(projects, pop_proj, conn = db_name)

    # failed <- import_node_data(conn,
    #                            outpath,
    #                            myproject=myproject)
  } else {
    # remote database (i.e. AWS RDS)
    # failed <- import_node_data(conn,
    #                            outpath,
    #                            myproject=myproject)
  }
  # faul <- which(!sapply(failed[[1]], is.null))
  # if (length(faul > 0)) {
  #   failed <- Map(`[`, failed, faul)
  #   # save(failed, file = file.path(outpath, "caught.RData"))
  # } else {
  #   failed <- "all good!"
  #   # save(failed, file = file.path(outpath, "caught.RData"))
  # }
}
