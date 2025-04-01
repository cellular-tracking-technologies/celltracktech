#' Remove duplicates
#'
#' This function allows you to remove records for more than one beep on the same node at the same time. Additionally, it gets rid of all records where RSSI varies for the same beep record
#' (combination of time, tag ID and node).
#' @param conn the connection to your local database
#' @export

db_cleanup <- function(conn) {
start <- Sys.time()
  print("getting rid of duplicate records")
  DBI::dbExecute(conn, "WITH ordered AS (
  SELECT id, time, upper(tag_id), upper(node_id), tag_rssi,
    rank() OVER (PARTITION BY time, upper(tag_id), upper(node_id), tag_rssi  ORDER BY id) AS rnk
  FROM raw where node_id is not null
),
to_delete AS (
  SELECT *
  FROM   ordered
  WHERE  rnk > 1
)
delete from raw using to_delete where raw.id = to_delete.id")
print(Sys.time() - start)
print("getting rid of duplicate node health records")
DBI::dbExecute(conn, "DELETE FROM node_health T1
USING node_health T2
WHERE T1.node_id is not null
AND T1.ctid < T2.ctid
AND  upper(T1.node_id) = upper(T2.node_id)
AND  T1.time = T2.time
AND  T1.radio_id = T2.radio_id")

print(Sys.time() - start)
print("getting rid of bad records")
DBI::dbExecute(conn, "WITH ordered AS (
  SELECT id, time, upper(tag_id) as tag, upper(node_id),
    rank() OVER (PARTITION BY time, upper(tag_id), upper(node_id)  ORDER BY id) AS rnk
  FROM raw where node_id is not null
),
to_delete AS (
  SELECT *
  FROM   ordered
  WHERE  rnk > 1
)

delete from raw using to_delete where raw.time = to_delete.time and upper(raw.node_id) = to_delete.upper and upper(raw.tag_id) =to_delete.tag")

print(Sys.time() - start)
print("getting rid of null tags")
DBI::dbExecute(conn, "delete from raw where tag_id is null")
#2022-04-04 19:43:43-04 1933552D 377c59

print(Sys.time() - start)
print("getting rid of bad nodes")
nodes <- DBI::dbReadTable(conn, "nodes")
badnodes <- toupper(nodes$node_id[(nchar(nodes$node_id) != 6 & nchar(nodes$node_id) != 8)])
goodnodes <- toupper(nodes$node_id[!(nchar(nodes$node_id) != 6 & nchar(nodes$node_id) != 8)])
sapply(badnodes, function(y) delnodes(conn, y))
badnodestr <- paste("'",badnodes, "'", sep="",collapse = ",") #test this...
DBI::dbExecute(conn, paste0("DELETE FROM raw where upper(node_id) in (",badnodestr,")"))
DBI::dbExecute(conn, paste0("DELETE FROM node_health where upper(node_id) in (",badnodestr,")"))
DBI::dbExecute(conn, paste0("delete from nodes where upper(node_id) in (", badnodestr, ")"))
DBI::dbExecute(conn, paste0("delete from blu where upper(node_id) in (", badnodestr, ")"))

print(Sys.time() - start)
print("updating node IDs to upper case")
insertnew <- DBI::dbSendQuery(conn, paste("INSERT INTO ", "nodes (node_id)", " VALUES ($1)
                                           ON CONFLICT DO NOTHING", sep = ""))
DBI::dbBind(insertnew, params = list(goodnodes))
DBI::dbClearResult(insertnew)
#DBI::dbExecute(conn, "update nodes set node_id = upper(node_id)")
DBI::dbExecute(conn, "update raw set node_id = upper(node_id)")
DBI::dbExecute(conn, "update node_health set node_id = upper(node_id)")

print(Sys.time() - start)
print("getting rid of duplicate lowercase nodes")
DBI::dbExecute(conn, "DELETE FROM nodes T1
USING nodes T2
WHERE (T1.node_id ~ '[a-z]') is true")

print(Sys.time() - start)
print("filling in missing files")
res <- DBI::dbGetQuery(conn, "select distinct path from gps")
res2 <- DBI::dbGetQuery(conn, "select distinct path from raw")
res1 <- DBI::dbGetQuery(conn, "select distinct path from node_health")
filesdone <- c(res$path, res1$path, res2$path)
allnode <- DBI::dbReadTable(conn, "data_file")
filesin <- allnode$path

filesdone <- filesdone[!filesdone %in% filesin]
insertnew <- DBI::dbSendQuery(conn, paste("INSERT INTO ", "data_file (path)", " VALUES ($) ON CONFLICT DO NOTHING", sep = ""))
DBI::dbBind(insertnew, params = list(unique(filesdone)))
DBI::dbClearResult(insertnew)
print("done")
print(Sys.time() - start)
}

#' Incorporate node data
#'
#' This function allows you to include data pulled directly from your nodes. Create a folder called "nodes" and create sub-folders named for each node ID.
#' Put the beep files pulled from the node(s) in their appropriate folder(s)
#' @param d The connection to your local database
#' @param outpath The overarching directory where your "nodes" folder lives, within your project folder (if applicable)
#' @param myproject (optional) If you have a sensor station related to this project, place your "nodes" folder in the folder named for your project as it
#' appears on the CTT website. If you use the API to pull sensor station data, this folder has been automatically created
#' @export
#' @examples
#' conn <- DBI::dbConnect(RPostgres::Postgres(), dbname="mydb")
#' outpath <- "~/Documents/my_project/"
#' list.files(outpath)
#' # "My Project"
#' import_node_data(conn, outpath, myproject="My Project")

import_node_data <- function(d, outpath, myproject=NULL, station_id) {
  myout <- outpath
  if(!is.null(myproject)) {
    myout <- file.path(outpath,myproject)
  }
  # myfiles <- list.files(file.path(myout, "nodes"), pattern="beep.*csv",recursive = TRUE, full.names = TRUE)
  myfiles <- list.files(file.path(myout, "nodes"),
                        pattern=".*csv",
                        recursive = TRUE,
                        full.names = TRUE)

  print(paste('myfiles', myfiles))
  files_loc <- sapply(strsplit(myfiles, "/"), tail, n=2)
  files <- paste(files_loc[1,],files_loc[2,],sep="/")
  print(paste('files', files))
  allnode <- DBI::dbReadTable(d, "data_file")
  filesdone <- allnode$path
  files_import <- myfiles[which(!files %in% filesdone)]
  print(files_import)
  lapply(files_import,
         load_node_data,
         conn=d,
         outpath=outpath,
         myproject=myproject,
         station_id = station_id)
}


#' Load Node Data
#'
#' @param e file path
#' @param conn database connection
#' @param outpath outpath directory
#' @param myproject project name
#' @param station_id sensor station id - string
#'
#' @returns
#' @export
#'
#' @examples
load_node_data <- function(e, conn, outpath, myproject, station_id) {
  #e <- file.path(outpath, "nodes", e)
  print(paste('e file', e))
  file <- tail(unlist(strsplit(e, "/")), n=2)
  print(paste('file', file))
  y <- paste(file, collapse="/")
  print(paste('y file', y))

  file_list = str_extract_all(y, c(regex('434(?=_)'),
                                   regex('(?<!_)(beep)'),
                                   regex('blu(?=_beep_\\d+)'),
                                   '2p4_ghz_beep',
                                   'gps',
                                   'health'))
  filetype = file_list %>% unlist()
  print(paste('filetype', filetype))

  sensor <- NA
  i <- DBI::dbReadTable(conn, "ctt_project_station")

  begin <- min(i$deploy_at)

  if(!is.null(myproject)) {
    myproj <- DBI::dbReadTable(conn, "ctt_project")
    projid <- myproj$id[which(myproj$name == myproject)]
    begin <- min(i$deploy_at[which(i$project_id == projid)])
  }

  if (length(begin) == 0) {
    begin <- as.POSIXct("2018-01-01")
  }

  #runs = split(seq_along(badlines), cumsum(c(0, diff(badlines) > 1)))
  #lapply(runs[lengths(runs) > 1], range)

  # get data from file
  df <- tryCatch({
    if (file.size(e) > 0) {
        read_csv(e, na = c("NA", ""), skip_empty_rows = TRUE)
    } else {
      err <- 'No Data in File'
      contents <- NULL

      z <- list(err, contents, y)

      return(z)
    }

    }, error = function(err) {
        # error handler picks up where error was generated
        print(paste("ignoring file", err, "- no data"))
        return(NULL)
    }, error = function(w) {
      print(paste('error in file', w))
      #x <- read.csv(e,header=TRUE,as.is=TRUE, na.strings=c("NA", ""), skipNul = TRUE) might need to reimplement this...
      #x <- rbind(x,Correct_Colnames(x))
      #colnames(x) <- c("time", "id", "rssi")
      #return(x)
    })

  # remove corrupted data
  df_bad = df %>%
    filter(if_any(everything(), ~ str_detect(., "[^\\x00-\\x7F]+") == TRUE))

  df_anti_join = anti_join(df, df_bad)
  df = df_anti_join
  # badlines <- grep("[^ -~]", df$id)

  if ('id' %in% colnames(df)) {
    badlines = grep("[^ -~]", df$id)
  } else if ('tag_id' %in% colnames(df)) {
    badlines = grep("[^ -~]", df$tag_id)
  } else {
    badlines = grep("[^ -~]", df$time)
  }

  # get warning message: unknown or uninitialised column: id
  if (length(badlines) > 0) {
    salvage <- df[badlines,]
    df <- df[-badlines,]
    DatePattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[T,\\.][[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?'
    salvage <- salvage[which(grepl(DatePattern,salvage$id)),]
    if (nrow(salvage) > 0) {
    fil <- tempfile("test")
    #MAY BREAK WITH MORE THAN 1 ROW
    cat(salvage$id, file = fil)
    addin <- readLines(fil)
    unlink(fil)

    badlines <- grep("[^ -~]", addin)
    addin <- addin[-badlines]
    fil <- tempfile()
    cat(addin, file = fil,
        sep = "\n")
    svarhis <- tryCatch({
      svarhis <- read.csv(fil, header=FALSE, col.names=c("time", "id", "rssi"))
      }, error = function(err) {
        # error handler picks up where error was generated
        data.frame()
      })
    unlink(fil)
    df <- rbind(df, svarhis)

    }
    #savethis <- regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[T,\\.][[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?.*",salvage$id)
  }

  badlines <- grep("[^ -~]", df$time)

  if (length(badlines) > 0) {
    df <- df[-badlines,]
  }

  badlines <- grep("[[:digit:]]-", df$time, invert=TRUE)

  if (length(badlines) > 0) {
    df <- df[-badlines,]
  }

  # filter out timestamps that are not timestamps
  if ('time' %in% colnames(df)) {
    df = df %>%
      mutate(Time = as.POSIXct(time,
                               format="%Y-%m-%dT%H:%M:%SZ",
                               tz = 'UTC',
                               optional=TRUE)) %>%
      filter(is.na(Time) == FALSE)
  } else {
    df = df %>%
      mutate(Time = as.POSIXct(Time,
                               format="%Y-%m-%dT%H:%M:%SZ",
                               tz = 'UTC',
                               optional=TRUE)) %>%
      filter(is.na(Time) == FALSE)
  }

  # dataframe is not null, modify df
  if(!is.null(df)) {

    # set Node ID
    df$NodeId <- tolower(file[1])

    # set time zone (always UTC)
    time = "UTC"

    df <- df[!is.na(df$Time),]
    df$time <- NULL
    df <- df[order(df$Time),]

    start <- min(df$Time, na.rm=T)
    end <- max(df$Time, na.rm=T)

    # filetype conditional, modify the test (data from database) and df (data from file) column names for the anti_join function
    if (filetype == 'gps') {

      df$node_id = toupper(df$NodeId)

      # get existing data table from database
      test <- dbGetQuery(conn,
                         paste0("SELECT * FROM node_gps ",
                                "WHERE time >= '", start,
                                "'AND time <= '", end, "'"))
      df$time = df$Time

      if ('OnTime' %in% colnames(df)) {
        df <- df %>%
          rename(
                 latitude = 'Latitude',
                 longitude = 'Longitude',
                 altitude = 'Altitude',
                 hdop = 'Hdop',
                 vdop = 'Vdop',
                 pdop = 'Pdop',
                 navigation_mode = 'NavigationMode',
                 satellites = 'Satellites',
                 on_time = 'OnTime')
      } else {
        df$navigation_mode = NA
        df$satellites = NA
      }

      # only gets beeps from node, and not ones picked up by sensor station

      # get other columns that exist in database
      df$path <- y
      df$station_id <- station_id

      df2 <- dplyr::anti_join(df, test, by = c('time', 'station_id', 'node_id'))

      # remove any duplicates with same gps_at value
      df3 <- df2 %>%
        distinct(time,
                 node_id,
                 station_id,
                 .keep_all = TRUE)

      z <- db_insert(contents=df3,
                     filetype='node_gps',
                     conn=conn,
                     y=y,
                     begin=begin)

    } else if (filetype == 'health') {
      # get existing data table from database
      test <- dbGetQuery(conn,
                         paste0("SELECT * FROM node_health_from_node ",
                                "WHERE time >= '", start,
                                "'AND time <= '", end, "'"))

      df$node_id = toupper(df$NodeId)
      df$station_id = station_id
      df$path = y
      df$time = df$Time

      if ('sub_ghz_det' %in% colnames(df)) {
        df$sub_ghz_det = df$sub_ghz_det
      } else {
        df$sub_ghz_det = df$`434_det`
      }

      if ('ble_det' %in% colnames(df)) {
        df$ble_det = df$ble_det
      } else {
        df$ble_det = df$blu_det
      }

      # only gets beeps from node, and not ones picked up by sensor station
      df2 <- df %>%
        distinct(time,
                 node_id,
                 path,
                 .keep_all = TRUE)

      df3 = dplyr::anti_join(df2, test, by = c('time', 'station_id', 'node_id'))

      z <- db_insert(contents=df3,
                     filetype='node_health_from_node',
                     conn=conn,
                     y=y,
                     begin=begin)

    } else if (filetype == 434 || filetype == 'beep') {

      # rename id to tag_id
      if ('tag_id' %in% colnames(df)) {
        df$tag_id = toupper(df$tag_id)
      } else {
        df$tag_id = toupper(df$id)
      }

      if ('id' %in% colnames(df)) {
        df <- df %>%
          mutate(id = NULL)
      }

      df$tag_rssi <- as.integer(df$rssi)
      df <- df[!is.na(df$tag_rssi),]
      df$rssi <- NULL

      start <- min(df$Time, na.rm=T)
      end <- max(df$Time, na.rm=T)
      print(paste(start, end))

      # get existing data table from database
      test <- dbGetQuery(conn,
                         paste0("SELECT * FROM node_raw ",
                                "WHERE Time > '", start,
                                "' and Time < '", end, "'"))

      df$path = y
      df$station_id = station_id
      df$node_id = df$NodeId
      df$time = df$Time
      df$radio_id = 4
      df$validated = NA

      df2 <- dplyr::anti_join(df,test)

      z <- db_insert(contents=df2,
                     filetype='node_raw',
                     conn=conn,
                     y=y,
                     begin=begin)

    } else if (filetype == 'blu' || filetype == '2p4_ghz_beep') {

      df$tag_rssi <- as.integer(df$rssi)
      df <- df %>%
        filter(is.na(tag_rssi) == FALSE)
      df$time <- df$Time

      # revision (type of blu tag) is from sensor station, payload_version is from node
      if ('revision' %in% colnames(df)) {
        df$revision = df$revision
      } else {
        df$revision = df$payload_version
      }
      df$product = df$family

      start <- min(df$time, na.rm=T)
      end <- max(df$time, na.rm=T)
      print(paste(start, end))

      # get existing data table from database
      test <- dbGetQuery(conn,
                         paste0("SELECT * FROM node_blu ",
                                "WHERE time > '", start,
                                "'AND time < '", end, "'"))

      # only get records that do not exist in database
      df$path = y
      df$station_id = station_id
      df$node_id = df$NodeId
      df$radio_id = NA
      df$usb_port = NA
      df$blu_radio_id = NA

      df2 <- dplyr::anti_join(df, test)

      z <- db_insert(contents=df2,
                     filetype='node_blu',
                     conn=conn,
                     y=y,
                     begin=begin)

    }
  }
}

Correct_Colnames <- function(df) {
  rowval <- gsub("^X\\.", "-",  colnames(df))
  rowval <- gsub("^X", "",  rowval)
  DatePattern = '^[[:digit:]]{4}\\.[[:digit:]]{2}\\.[[:digit:]]{2}[T,\\.][[:digit:]]{2}\\.[[:digit:]]{2}\\.[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?'
  #rowval[which(grepl(DatePattern,rowval))] <- as.character(as.POSIXct(rowval[grepl(DatePattern,rowval)], format="%Y.%m.%d.%H.%M.%S", tz="UTC"))
  return(rowval)
}

# Function to remove non-ASCII characters using iconv
remove_non_ascii <- function(x) {
  if (str_detect(x, "[^\\x00-\\x7F]") == TRUE) {

  }
}

# combine node data function
combine_node_data() {

  raw = DBI::dbGetQuery(conn, 'SELECT * FROM raw')

  raw_dplyr = raw %>%
    mutate(radio_id = as.numeric(radio_id))%>%
    bind_rows(df2) %>%
    group_by(time, tag_id, station_id, node_id) %>%
    distinct(time, tag_id, station_id, node_id, .keep_all = TRUE)

  DBI::dbWriteTable(con, 'raw_combine', raw_dplyr, overwrite = TRUE)

  dbSendQuery(con, 'ALTER TABLE raw_combine
                  ALTER COLUMN time TYPE TIMESTAMP WITH TIME ZONE')

  remove(raw)
  remove(raw_dplyr)

  blu = DBI::dbGetQuery(conn, 'SELECT * FROM blu')

  blu_dplyr = blu %>%
    bind_rows(df2) %>%
    group_by(time, tag_id, station_id, node_id) %>%
    distinct(time, tag_id, station_id, node_id, .keep_all = TRUE)

  DBI::dbWriteTable(con, 'blu', blu_dplyr, overwrite = TRUE)

  dbSendQuery(con,
              'ALTER TABLE blu
                  ALTER COLUMN time TYPE TIMESTAMP WITH TIME ZONE')

  remove(blu)
  remove(blu_dplyr)
}
