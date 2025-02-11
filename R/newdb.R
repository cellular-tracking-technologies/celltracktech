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

import_node_data <- function(d, outpath, myproject=NULL) {
  myout <- outpath
  if(!is.null(myproject)) {myout <- file.path(outpath,myproject)}
  # myfiles <- list.files(file.path(myout, "nodes"), pattern="beep.*csv",recursive = TRUE, full.names = TRUE)
  myfiles <- list.files(file.path(myout, "nodes"), pattern=".*csv",recursive = TRUE, full.names = TRUE)

  print(paste('myfiles',myfiles))
  files_loc <- sapply(strsplit(myfiles, "/"), tail, n=2)
  files <- paste(files_loc[1,],files_loc[2,],sep="/")
  allnode <- DBI::dbReadTable(d, "data_file")
  filesdone <- allnode$path
  files_import <- myfiles[which(!files %in% filesdone)]
  lapply(files_import, load_node_data, conn=d, outpath=outpath, myproject=myproject)
}

load_node_data <- function(e, conn, outpath, myproject) {
  Correct_Colnames <- function(df) {
    rowval <- gsub("^X\\.", "-",  colnames(df))
    rowval <- gsub("^X", "",  rowval)
    DatePattern = '^[[:digit:]]{4}\\.[[:digit:]]{2}\\.[[:digit:]]{2}[T,\\.][[:digit:]]{2}\\.[[:digit:]]{2}\\.[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?'
    #rowval[which(grepl(DatePattern,rowval))] <- as.character(as.POSIXct(rowval[grepl(DatePattern,rowval)], format="%Y.%m.%d.%H.%M.%S", tz="UTC"))
    return(rowval)
  }

  #e <- file.path(outpath, "nodes", e)
  file <- tail(unlist(strsplit(e, "/")), n=2)
  print(paste('file', file))
  y <- paste(file, collapse="/")
  sensor <- NA
  i <- DBI::dbReadTable(conn, "ctt_project_station")
  print(paste('load node data i', i))

  begin <- min(i$deploy_at)
  print(paste('load node data begin', begin))

  if(!is.null(myproject)) {
    myproj <- DBI::dbReadTable(conn, "ctt_project")
    print(paste('load node data y', myproj))

    projid <- myproj$id[which(myproj$name == myproject)]
    print(paste('load node data projid', projid))

    begin <- min(i$deploy_at[which(i$project_id == projid)])
    print(paste('load node data begin, second', begin))

  }

  if(length(begin) == 0) {begin <- as.POSIXct("2018-01-01")}
  filetype <- "raw"
  #runs = split(seq_along(badlines), cumsum(c(0, diff(badlines) > 1)))
  #lapply(runs[lengths(runs) > 1], range)
  df <- tryCatch({
    if (file.size(e) > 0) {
        # print(paste('read csv', readr::read_csv(e,na=c("NA", ""), skip_empty_rows = TRUE)))
        read_csv(e,na=c("NA", ""), skip_empty_rows = TRUE)
    }}, error = function(err) {
        # error handler picks up where error was generated
        print(paste("ignoring file", err, "- no data"))
        return(NULL)
    }, error = function(w) {
      # print("error in file")
      print(paste('error in file', w))
      #x <- read.csv(e,header=TRUE,as.is=TRUE, na.strings=c("NA", ""), skipNul = TRUE) might need to reimplement this...
      #x <- rbind(x,Correct_Colnames(x))
      #colnames(x) <- c("time", "id", "rssi")
      #return(x)
    })
  badlines <- grep("[^ -~]", df$id)
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
  if (length(badlines) > 0) { df <- df[-badlines,]}
  badlines <- grep("[[:digit:]]-", df$time, invert=TRUE)
  if (length(badlines) > 0) { df <- df[-badlines,]}
    #if(!all((c("time", "id", "rssi") %in% colnames(df)))) {df <- NULL}
  if(!is.null(df)) {
    df$NodeId <- tolower(file[1])
    time = "UTC"
    df$Time <- df$time
    if(is.character(df$time)) {df$Time <- as.POSIXct(df$time,format="%Y-%m-%dT%H:%M:%SZ",tz = time, optional=TRUE)}
    df <- df[!is.na(df$Time),]
    df$time <- NULL
  #nodes <- nodes[nodes$Time > as.POSIXct("2020-08-20"),]
    df <- df[order(df$Time),]
    df$RadioId <- 4 #https://bitbucket.org/cellulartrackingtechnologies/lifetag-system-report/src/master/beeps.py
    # df$TagId <- toupper(df$id)
    df$TagId <- ifelse("id" %in% colnames(df), toupper(df$id), toupper(df$tag_id))
    # if ('payload' %in% colnames(df)) {
    #   toupper(df$payload)
    # } else {
    #   df$payload = NA
    # }
    # print(colnames(df))
    df$payload <- ifelse('payload' %in% colnames(df), toupper(df$payload), NA)
    # print(paste('Blu Tag id', head(df$TagId)))
    df$id <- NULL
    df$TagRSSI <- as.integer(df$rssi)
    df <- df[!is.na(df$TagRSSI),]
    df$rssi <- NULL
    df$Validated <- 0
    validated <- which(nchar(df$TagId) == 10)
    df$Validated[validated] <- 1
    df$TagId[validated] <- substr(df$TagId[validated], 1, 8)

    start <- min(df$Time, na.rm=T)
    end <- max(df$Time, na.rm=T)

    test <- dbGetQuery(conn, paste0("select * from raw where time > '", start,"' and time < '",end,"'"))

    test$Time <- test$time
    test$TagId <- test$tag_id
    test$RadioId <- test$radio_id
    test$NodeId <- toupper(test$node_id)
    test$TagRSSI <- test$tag_rssi
    test$Validated <- test$validated
    # test <- test[,colnames(df)] # grabs columns only from df... which do not contain path or station id

    df <- dplyr::anti_join(df,test) # only gets beeps from node, and not ones picked up by sensor station
    # colnames(df) = c('node_id', 'time', 'radio_id', 'tag_id', 'tag_rssi', 'validated', 'payload')
    df = df %>%
      dplyr::mutate(tag_id = NULL) %>%
      dplyr::rename(time = Time,
                    tag_rssi = TagRSSI,
                    radio_id = RadioId,
                    validated = Validated,
                    node_id = NodeId,
                    tag_id = TagId)
    df$path = y
    df$station_id = NA
    print(paste('df colnames', colnames(df)))

    # z <- db_insert(contents=df, filetype=filetype, conn=conn, sensor=sensor, y=y, begin=begin)
    z <- db_insert(contents=df, filetype=filetype, conn=conn, y=y, begin=begin)

    }
}
