#' Combine Detection Data and Sidekick Calibration Data
#'
#' @param test Sidekick calibration dataframe (loaded from file)
#' @param testdata_in Tag detection dataframe (from 'raw' data table)
#' @param nodes Dataframe of Nodes with lat/lon
#' @param tag_col Tag ID column
#' @param tagid Specific Tag ID you want to use for analysis
#' @param time_col Time column
#' @param timezone Timezone, set to UTC
#' @param x Longitude
#' @param y Latitude
#' @param node_ids NULL
#' @param loc_precision Location precision
#' @param latlon Boolean, set to True
#' @param fileloc filepath
#' @param filetype Data filetype; from CTT account (raw, blu)
#'
#' @returns combined.data - dataframe
#' @export
#'
#' @examples
#' combined_data <- data.setup(mytest,
#'                             testdata,
#'                             nodes,
#'                             tag_col = "tag_id",
#'                             tagid = "072A6633",
#'                             time_col = "Time",
#'                             timezone = "UTC",
#'                             x = "lon",
#'                             y = "lat",
#'                             loc_precision = 6,
#'                             fileloc = "./data/Meadows V2/meadows.duckdb",
#'                             filetype = "raw")
data.setup <- function(test,
                       testdata_in,
                       nodes,
                       tag_col,
                       tagid,
                       time_col,
                       timezone,
                       x,
                       y,
                       node_ids = NULL,
                       loc_precision = NA,
                       latlon=T,
                       fileloc,
                       filetype) {
  test$tag_id <- test[,tag_col]
  #test$tag_id <- test[,tag_col]
  test <- test[test$tag_id %in% tagid,]
  outtime <- lapply(time_col, out, contents=test, timezone=timezone)
  test[time_col] <- outtime
  if(length(time_col) < 2) {
    test$time <- test[,time_col]
    if(is.na(loc_precision)) {
      if(any(colnames(test) == "beep_number")) {
        test$TestId <- seq(1:nrow(test))
        test.info <- test
        #paste(format(test$Time, "%Y-%m-%d %H:%M"), test$beep_number, sep="_")
        test.info$Start.Time <- test.info$time - 1
        test.info$Stop.Time <- test.info$time + 1
        test.info$lat <- test[,y]
        test.info$lon <- test[,x]
      }
    } else {
      multfactor = 10^loc_precision
      test$lat <- format(trunc(test[,y]*multfactor)/multfactor,
                         nsmall=loc_precision)
      test$lon <- format(trunc(test[,x]*multfactor)/multfactor,
                         nsmall=loc_precision)
      test$id <- paste(test$lat, test$lon, sep="_")
      test <- test %>%
        mutate(c_diff = ifelse(id != lag(id), 1, 0))
      test$c_diff[1] <- 0
      test$TestId <- cumsum(test$c_diff)
      print('test')
      print(test)
      test.info <- setDT(test)[, .(Start.Time = min(time),
                                   Stop.Time = max(time)), by = TestId]

      test.info$id <- test$id[match(test.info$TestId,
                                    test$TestId)]
      testid <- test[!duplicated(test$id),]
      test.info$TestId <- testid$TestId[match(test.info$id,
                                              testid$id)]
      test.info$lat <- as.numeric(test$lat[match(test.info$TestId,
                                                 test$TestId)])
      test.info$lon <- as.numeric(test$lon[match(test.info$TestId,
                                                 test$TestId)])
    }
  } else {
    test.info <- test
    test.info$lat <- test[,y]
    test.info$lon <- test[,x]
  }

  test.UTM <- test.info %>%
    dplyr::group_by(TestId) %>%
    dplyr::slice_head(n=1)

  #if(length(tagid) > 1) {
  df1 <- test %>%
    group_by(TestId) %>%
    summarise(tag_id = list(unique(tagid))) %>%
    unnest(tag_id)

  test.info <- left_join(test.info, df1)
  #}
  print('test info')
  print(test.info)

  test.info$Start.Time <- test.info$Start.Time - 2
  test.info$Stop.Time <- test.info$Stop.Time + 2
  start <- min(test.info$Start.Time)
  end <- max(test.info$Stop.Time)

  #con <- DBI::dbConnect(duckdb::duckdb(), dbdir = fileloc, read_only = TRUE)
  testdata <- testdata_in %>%
    filter(time >= start & time <= end) |>
    filter(tag_id %in% tagid) |>
    collect()

  print('test data')
  print(testdata)
  #testdata$syncid <- paste(format(testdata$time, "%Y-%m-%d %H:%M"), testdata$sync, sep="_")

  start_buff = start - 2*60*60
  end_buff = end + 2*60*60

  #nodes <- tbl(con, "node_health") |>
  #  filter(time >= start_buff  & time <= end_buff) |>
  #  collect()

  #DBI::dbDisconnect(con)

  test.dat <- setDT(testdata)[test.info,
                              TestId := +(i.TestId),
                              on = .(tag_id, time >= Start.Time,
                                     time <= Stop.Time), by = .EACHI]
  print('test dat')
  print(test.dat)

  test.dat <- test.dat[!is.na(test.dat$TestId),]



  summary.test.tags <- test.dat %>%
    dplyr::group_by(node_id, TestId) %>%
    dplyr::summarise(avgRSS = mean(tag_rssi),
                     sdRSS = sd(tag_rssi),
                     n.det = n())

  if (!is.null(node_ids)) {
    nodes <- nodes[nodes$node_id %in% node_ids,]
    }

  #nodes <- node_file(nodes)

  dst <- raster::pointDistance(test.UTM[,c("lon", "lat")],
                               nodes[,c("node_lng", "node_lat")],
                               lonlat = latlon,
                               allpairs = T)
  dist_df <- data.frame(dst,
                        row.names = test.UTM$TestId)
  colnames(dist_df) <- nodes$node_id
  dist_df$TestId <- as.integer(rownames(dist_df))

  dist.gather <- dist_df %>%
    tidyr::gather(key = "node_id", value = "distance", -TestId)

  summary.dist <- summary.test.tags %>%
    dplyr::left_join(dist.gather)

  # Add UTMs of nodes and test locations to dataset
  combined.data <- summary.dist %>%
    dplyr::left_join(nodes[, c("node_id", "node_lat", "node_lng")]) %>%
    dplyr::left_join(test.UTM[, c("TestId", "lat", "lon")])

  return(combined.data)
}
