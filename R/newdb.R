#' Remove duplicates
#'
#' This function allows you to remove records for more than one beep on the same node at the same time. Additionally, it gets rid of all records where RSSI varies for the same beep record
#' (combination of time, tag ID and node).
#' @param conn the connection to your local database
#' @export

db_cleanup <- function(conn) {
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

delete from raw using to_delete where raw.time = to_delete.time and upper(raw.node_id) = to_delete.upper and upper(raw.tag_id) =to_delete.tag") #2022-04-04 19:43:43-04 1933552D 377c59
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
  myfiles <- list.files(file.path(myout, "nodes"), pattern="beep.*csv",recursive = TRUE, full.names = TRUE)
  print(myfiles)
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
    return(rowval)}

  #e <- file.path(outpath, "nodes", e)
  print(e)
  file <- tail(unlist(strsplit(e, "/")), n=2)
  y <- paste(file, collapse="/")
  sensor <- NA
  i <- DBI::dbReadTable(conn, "ctt_project_station")
  begin <- min(i$deploy_at)
  if(!is.null(myproject)) {
    myproj <- DBI::dbReadTable(conn, "ctt_project")
    projid <- myproj$id[which(myproj$name == myproject)]
    begin <- min(i$deploy_at[which(i$project_id == projid)])
  }

  if(length(begin) == 0) {begin <- as.POSIXct("2018-01-01")}
  filetype <- "raw"
  #runs = split(seq_along(badlines), cumsum(c(0, diff(badlines) > 1)))
  #lapply(runs[lengths(runs) > 1], range)
  df <- tryCatch({
    if (file.size(e) > 0) {
        read.csv(e,header=TRUE,as.is=TRUE, na.strings=c("NA", ""), colClasses=c("id"="character"), skipNul = TRUE)
    }}, error = function(err) {
        # error handler picks up where error was generated
        print("ignoring file", e, "- no data")
        return(NULL)
    }, warning = function(w) {

      x <- read.csv(e,header=TRUE,as.is=TRUE, na.strings=c("NA", ""), skipNul = TRUE)
      x <- rbind(x,Correct_Colnames(x))
      colnames(x) <- c("time", "id", "rssi")
      return(x)
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
    df$Time <- as.POSIXct(df$time,format="%Y-%m-%dT%H:%M:%SZ",tz = time, optional=TRUE)
    df <- df[!is.na(df$Time),]
    df$time <- NULL
  #nodes <- nodes[nodes$Time > as.POSIXct("2020-08-20"),]
    df <- df[order(df$Time),]
    df$RadioId <- 4 #https://bitbucket.org/cellulartrackingtechnologies/lifetag-system-report/src/master/beeps.py
    df$TagId <- toupper(df$id)
    df$id <- NULL
    df$TagRSSI <- as.integer(df$rssi)
    df <- df[!is.na(df$TagRSSI),]
    df$rssi <- NULL
    df$Validated <- 0
    validated <- which(nchar(df$TagId) == 10)
    df$Validated[validated] <- 1
    df$TagId[validated] <- substr(df$TagId[validated], 1, 8)
    z <- db_insert(df, filetype, conn, sensor, y, begin)}
}
