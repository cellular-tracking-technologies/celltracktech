###########################################################################################################################################################
## Jessica Gorzo & Kristina Paxton
## 9/26/2024
##
## This is an updated version of the code base associated with Paxton et al. 2021 https://doi.org/10.5066/P94LQWIE
## Here's what's new...
## 1. The methods for shaping and re-sampling a calibration data set have been updated with respect to inputting a CTT Sidekick input file, or more broadly,
##    potentially with use for walking a transect and having a time stamp associated with a location, as opposed to specifying start and end time for a
##    location. Further, the methods for data shaping are updated to use time thresholds as bounds for matching recorded data
##
## 2. Functions have been redefined to more explicitly take passed input parameters, and there was some general code cleaning in keeping with R versioning
##    and e.g., related data type paradigms
##
## 3. Functionality has been included here to generate node locations from node health data
##
## 4. Variable naming conventions have been updated to reflect database headers (as opposed to raw or manually generated files)
##
## 5. Time formatting and cleaning for input files has been implemented
##
## 6. Location info has changed to the convention of lat/long
##########################################################################################################################################################

options(digits=9)

node_file <- function(health) {
  options(digits=9)

  if (nrow(health) < 1) stop("no node health data!")
  #health$timediff <- as.integer(health$time - health$recorded_at)
  #health <- health[health$timediff == 0,]
  health <- aggregate(health[,c("latitude", "longitude")],
                      list(health$node_id),
                      mean, na.rm=TRUE)

  if (any(is.na(health))) {
    health <- health[-which(is.na(health$latitude) || is.na(health$longitude)),]
  }

  colnames(health)[colnames(health)=="latitude"] <- "node_lat"
  colnames(health)[colnames(health)=="longitude"] <- "node_lng"
  colnames(health)[colnames(health)=="Group.1"] <- "node_id"
  return(health)
}

out <- function(x, contents, timezone) {
  x <- which(names(contents) == x)
  timecol <- contents[, x]
  if (is.character(timecol)) {
    DatePattern <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[T, ][[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?"
    exactDatePattern <- "^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[T, ][[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?$"
    brokenrow <- grep(exactDatePattern, timecol, invert = TRUE) # find row that has a date embedded in a messed up string (i.e. interrupted rows)
    timecol[brokenrow] <- substring(timecol[brokenrow], regexpr(DatePattern, timecol[brokenrow]))
    timecol[brokenrow[which(regexpr(DatePattern, timecol[brokenrow]) < 0)]] <- NA
    newtimecol <- as.POSIXct(timecol, tz = timezone)
  } else {
    newtimecol <- timecol
  }
  return(newtimecol)
}

#test <- mytest
#tag_col="Tag.Id"
#tagid="072A6633"
#time_col="Time..UTC."
#timezone="UTC"
#x="Longitude"
#y="Latitude"
#node_ids=node_ids
#loc_precision=6)
#fileloc <- "/home/jess/Documents/radio_projects/data/radio_projects/meadows/meadows.duckdb"
data.setup <- function(test, testdata_in, nodes, tag_col, tagid, time_col, timezone, x, y, node_ids = NULL, loc_precision = NA, latlon=T, fileloc, filetype) {
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
      test$lat <- format(trunc(test[,y]*multfactor)/multfactor, nsmall=loc_precision)
      test$lon <- format(trunc(test[,x]*multfactor)/multfactor, nsmall=loc_precision)
      test$id <- paste(test$lat, test$lon, sep="_")
      test <- test %>%
        mutate(c_diff = ifelse(id != lag(id), 1, 0))
      test$c_diff[1] <- 0
      test$TestId <- cumsum(test$c_diff)
      test.info <- setDT(test)[, .(Start.Time = min(time), Stop.Time = max(time)), by = TestId]
      test.info$id <- test$id[match(test.info$TestId, test$TestId)]
      testid <- test[!duplicated(test$id),]
      test.info$TestId <- testid$TestId[match(test.info$id, testid$id)]
      test.info$lat <- as.numeric(test$lat[match(test.info$TestId, test$TestId)])
      test.info$lon <- as.numeric(test$lon[match(test.info$TestId, test$TestId)])
    }
  } else {
    test.info <- test
    test.info$lat <- test[,y]
    test.info$lon <- test[,x]}

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

  test.info$Start.Time <- test.info$Start.Time - 2
  test.info$Stop.Time <- test.info$Stop.Time + 2
  start <- min(test.info$Start.Time)
  end <- max(test.info$Stop.Time)

  #con <- DBI::dbConnect(duckdb::duckdb(), dbdir = fileloc, read_only = TRUE)
  testdata <- testdata_in %>%
    filter(time >= start & time <= end) |>
    filter(tag_id %in% tagid) |>
    collect()

  #testdata$syncid <- paste(format(testdata$time, "%Y-%m-%d %H:%M"), testdata$sync, sep="_")

  start_buff = start - 2*60*60
  end_buff = end + 2*60*60

  #nodes <- tbl(con, "node_health") |>
  #  filter(time >= start_buff  & time <= end_buff) |>
  #  collect()

  #DBI::dbDisconnect(con)

  test.dat <- setDT(testdata)[test.info,  TestId := +(i.TestId), on = .(tag_id, time > Start.Time, time < Stop.Time), by = .EACHI]
  test.dat <- test.dat[!is.na(test.dat$TestId),]

  summary.test.tags <- test.dat %>%
    dplyr::group_by(node_id, TestId) %>%
    dplyr::summarise(avgRSS = mean(tag_rssi),
                     sdRSS = sd(tag_rssi),
                     n.det = n())

  if(!is.null(node_ids)) {nodes <- nodes[nodes$node_id %in% node_ids,]}

  #nodes <- node_file(nodes)

  dst <- raster::pointDistance(test.UTM[,c("lon", "lat")], nodes[,c("node_lng", "node_lat")], lonlat = latlon, allpairs = T)
  dist_df <- data.frame(dst, row.names = test.UTM$TestId)
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
  return(combined.data)}

estimate.distance <- function(x, K, a, S) {

  # supress warnings
  options(warn = -1)

  # Calculate estimated distance based on RSSI~Distance relationship and indicate simulation round
  combined.data <- x %>%
    dplyr::mutate(e.dist = (log(avgRSS - K) - log(a)) / -S)

  # Remove rows with NAs
  combined.data <- combined.data[!is.na(combined.data$e.dist),]

  # Change negative distances to 10 (rss values > intercept of exponential curve and thus negative) - indicates very close to the node
  combined.data <- combined.data %>%
    dplyr::mutate(e.dist = dplyr::case_when(e.dist < 0 ~ 10,
                                            e.dist >=0 ~ e.dist))

  # Save File to outpath
  #write.csv(combined.data, paste0(outpath, "LocError_Dataset.csv"),
  #          row.names = F)

  return(combined.data)

}

trilateration.TestData.NoFilter <- function(x) {

  # supress warnings
  options(warn = -1)

  # make a vector of unique trilaterations to run
  tests = unique(x$TestId)

  # Make a dataframe with only 1 row per test
  test.UTM <- x %>%
    dplyr::group_by(TestId) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::select(TestId, lat, lon)

  # Create a dataframe for output estimates
  estimated.location_results <- data.frame(TestId=character(), No.Nodes = numeric(), x.est=numeric(), y.est=numeric(),
                                           x.ci.lower =numeric(), x.ci.upper =numeric(), y.ci.lower = numeric(), y.ci.upper = numeric())


  for(j in 1:length(tests)) {

    # Isolate the test
    sub.test <- x %>% dplyr::filter(TestId == tests[j])

    # Determine the node with the strongest RSS value
    max.RSS <- sub.test[which.max(sub.test$avgRSS),]

    # Calculate no nodes for the test
    no.nodes <- dplyr::n_distinct(sub.test$node_id)


    # To deal with potential errors where the model fails due to bad starting values using tryCatch everything you want evaluated by tryCatch goes inside {},
    # then the error will be printed but the loop will continue

    # Non-linear test to optimize the location of unknown signal by looking at the radius around each Node based on estimated distance and the pairwise distance between all nodes
    tryCatch( {
      nls.test <- nls(e.dist ~ raster::pointDistance(data.frame(node_lng, node_lat), c(NodeUTMx_solution, NodeUTMy_solution), lonlat = T, allpairs = T),
                               data = sub.test, start=list(NodeUTMx_solution=max.RSS$node_lng, NodeUTMy_solution=max.RSS$node_lat),
                               control=nls.control(warnOnly = T, minFactor=1/30000, maxiter = 100)) # gives a warning, but doesn't stop the test from providing an estimate based on the last iteration before the warning



    # Determine an error around the point location estimate
    par.est = cbind(coef(nls.test), nlstools::confint2(nls.test))
    lng.ci.upper =  par.est[1,3]
    lng.ci.lower =  par.est[1,2]
    lat.ci.upper =  par.est[2,3]
    lat.ci.lower =  par.est[2,2]}

    ,error = function(e)  {cat("ERROR :",conditionMessage(e), "\n")})

    # estimated location of the point and error
    estimated.loc <- data.frame(TestId = tests[j], No.Nodes = no.nodes, x.est = par.est[1,1], y.est = par.est[2,1],
                                x.ci.lower = lng.ci.lower, x.ci.upper = lng.ci.upper,  y.ci.lower = lat.ci.lower, y.ci.upper = lat.ci.upper)

    # Populate dataframe with results
    estimated.location_results <- rbind(estimated.location_results, estimated.loc)

  }

  # combine estimated locations with true locations
  combined_results <- estimated.location_results %>%
    dplyr::left_join(test.UTM)

  # Calculate difference distance between estimated and true location
  dst <- raster::pointDistance(combined_results[,c("lon", "lat")], combined_results[,c(3:4)], lonlat = T, allpairs = F)

  # bring all together
  combined_results_final <- dplyr::bind_cols(combined_results, data.frame(diff.dist = dst)) %>%
    dplyr::mutate(filter = "No Filter")

  # save file
  #write.csv(combined_results_final, paste0(outpath, "Trilateration.TestData_NoFilter.RSS_Results.csv"),  row.names = F)

  # summarize statitics for a given filter
  summary.stats <- combined_results_final %>%
    dplyr::summarise(n.est.tests = dplyr::n_distinct(TestId),
                     avg.no.nodes = mean(No.Nodes),
                     avg.diff = mean(diff.dist),
                     sd.diff = sd(diff.dist),
                     lower.ci = avg.diff - qnorm(0.975)*sd.diff/sqrt(n.est.tests),
                     upper.ci = avg.diff + qnorm(0.975)*sd.diff/sqrt(n.est.tests),
                     med.diff = median(diff.dist),
                     min.diff = min(diff.dist),
                     max.dist = max(diff.dist)) %>%
    dplyr::mutate(filter = "No Filter")

  # save file
  #write.csv(summary.stats, paste0(outpath, "Trilateration.TestData_NoFilter_Summary.Stats.csv"),  row.names = F)

  return(list(combined_results_final,summary.stats))
}

trilateration.TestData.RSS.Filter <- function(x, RSS.FILTER) {

  # supress warnings
  options(warn = -1)

  # Empty data frame to populate with summary results
  summary.stats_results <- data.frame(n.est.tests = numeric(), avg.no.nodes = numeric(), avg.diff = numeric(), sd.diff = numeric(),
                                      lower.ci = numeric(), upper.ci = numeric(), med.diff = numeric(),
                                      min.diff = numeric(), max.diff = numeric(), filter = character())

  # Make a dataframe with only 1 row per test
  test.UTM <- x %>%
    dplyr::group_by(TestId) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::select(TestId, lat, lon)

  for (i in 1:length(RSS.FILTER)) {

    # Remove RSSI <= filter
    combined.data <- x %>%
      dplyr::filter(avgRSS >= RSS.FILTER[i])

    # Filter list so only contains tests which had <3 nodes detecting the transmitter
    sample.size <- combined.data %>%
      dplyr::group_by(TestId) %>%
      dplyr::summarise(n.nodes = n()) %>%
      dplyr::filter(n.nodes < 3)

    # Remove Tests with <3 nodes picking up a signal > value indicated
    combined.data.red <- combined.data %>%
      dplyr::filter(!(TestId %in% sample.size$TestId))

    # make a vector of unique trilaterations to run
    tests = unique(combined.data.red$TestId)

    # Create a dataframe for output estimates
    estimated.location_results <- data.frame(TestId=character(), No.Nodes = numeric(), x.est=numeric(), y.est=numeric(),
                                             x.ci.lower =numeric(), x.ci.upper =numeric(), y.ci.lower = numeric(), y.ci.upper = numeric())


    for(j in 1:length(tests)) {

      # Isolate the test
      sub.test <- combined.data.red %>% dplyr::filter(TestId == tests[j])

      # Determine the node with the strongest RSS value
      max.RSS <- sub.test[which.max(sub.test$avgRSS),]

      # Calculate no nodes for the test
      no.nodes <- dplyr::n_distinct(sub.test$node_id)


      # To deal with potential errors where the model fails due to bad starting values using tryCatch everything you want evaluated by tryCatch goes inside {},
      # then the error will be printed but the loop will continue

      # Non-linear test to optimize the location of unknown signal by looking at the radius around each Node based on estimated distance and the pairwise distance between all nodes
      tryCatch( {
        nls.test <- nls(e.dist ~ raster::pointDistance(data.frame(node_lng, node_lat), c(NodeUTMx_solution, NodeUTMy_solution), lonlat = T, allpairs = T),
                                 data = sub.test, start=list(NodeUTMx_solution=max.RSS$node_lng, NodeUTMy_solution=max.RSS$node_lat),
                                 control=nls.control(warnOnly = T, minFactor=1/30000, maxiter = 100)) # gives a warning, but doesn't stop the test from providing an estimate based on the last itteration before the warning



      # Determine an error around the point location estimate
      par.est = cbind(coef(nls.test), nlstools::confint2(nls.test))
      lng.ci.upper =  par.est[1,3]
      lng.ci.lower =  par.est[1,2]
      lat.ci.upper =  par.est[2,3]
      lat.ci.lower =  par.est[2,2]}

      ,error = function(e)  {cat("ERROR :",conditionMessage(e), "\n")})

      # estimated location of the point and error
      estimated.loc <- data.frame(TestId = tests[j], No.Nodes = no.nodes, x.est = par.est[1,1], y.est = par.est[2,1],
                                  x.ci.lower = lng.ci.lower, x.ci.upper = lng.ci.upper,  y.ci.lower = lat.ci.lower, y.ci.upper = lat.ci.upper)

      # Populate dataframe with results
      estimated.location_results <- rbind(estimated.location_results, estimated.loc)

    }

    # combine estimated locations with true locations
    combined_results <- estimated.location_results %>%
      dplyr::left_join(test.UTM)

    # Calculate difference distance between estimated and true location
    dst <- raster::pointDistance(combined_results[,c("lon", "lat")], combined_results[,c(3:4)], lonlat = T, allpairs = F)

    # bring all together
    combined_results_final <- dplyr::bind_cols(combined_results, data.frame(diff.dist = dst)) %>%
      dplyr::mutate(filter = paste("RSS", RSS.FILTER[i], sep="_"))

    # save file
    #write.csv(combined_results_final, paste0(outpath, "Trilateration.Test.Data_Filter.RSS.", RSS.FILTER[i], "_Results.csv"),  row.names = F)

    # summarize statitics for a given filter
    summary.stats <- combined_results_final %>%
      dplyr::summarise(n.est.tests = dplyr::n_distinct(TestId),
                       avg.no.nodes = mean(No.Nodes),
                       avg.diff = mean(diff.dist),
                       sd.diff = sd(diff.dist),
                       lower.ci = avg.diff - qnorm(0.975)*sd.diff/sqrt(n.est.tests),
                       upper.ci = avg.diff + qnorm(0.975)*sd.diff/sqrt(n.est.tests),
                       med.diff = median(diff.dist),
                       min.diff = min(diff.dist),
                       max.dist = max(diff.dist)) %>%
      dplyr::mutate(filter = paste("RSS", RSS.FILTER[i], sep="_"))

    # write summary to empty dataframe
    summary.stats_results <- rbind(summary.stats_results, summary.stats)


  }

  # save file
  #write.csv(summary.stats_results, paste0(outpath, "Trilateration.Test.Data_Filters.RSSI_Summary.Stats.csv"),  row.names = F)

  return(list(combined_results_final,summary.stats_results))

}

trilateration.TestData.Distance.Filter <- function(x, DIST.FILTER){

  # supress warnings
  options(warn = -1)

  # Make a dataframe with only 1 row per test
  test.UTM <- x %>%
    dplyr::group_by(TestId) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::select(TestId, lat, lon)

  # make a vector of unique trilaterations to run
  tests = unique(x$TestId)
  nodes <- x[!duplicated(x$node_id),]
  # Calculate distance between nodes
  dist.nodes <- raster::pointDistance(nodes[,c("node_lng", "node_lat")], nodes[,c("node_lng", "node_lat")], lonlat = F, allpairs = T)

  # Make matrix into a dataframe with a row for NodeId
  dist.nodes_df <- data.frame(dist.nodes, row.names = nodes$node_id)
  colnames(dist.nodes_df) <- nodes$node_id
  dist.nodes_df$NodeId <- rownames(dist.nodes_df)


  # Empty data frame to populate with summary results
  summary.stats_results <- data.frame(n.est.tests = numeric(), avg.no.nodes = numeric(), avg.diff = numeric(), sd.diff = numeric(),
                                      lower.ci = numeric(), upper.ci = numeric(), med.diff = numeric(),
                                      min.diff = numeric(), max.diff = numeric(), filter = character())


  # loop through distances

  for(i in 1:length(DIST.FILTER)) {

    # Create a dataframe for output estimates
    estimated.location_results <- data.frame(TestId=character(), No.Nodes = numeric(), x.est=numeric(), y.est=numeric(),
                                             x.ci.lower =numeric(), x.ci.upper =numeric(), y.ci.lower = numeric(), y.ci.upper = numeric())

    # identify the distance
    dist.no <- DIST.FILTER[i]


    # Loop through random points
    for(j in 1:length(tests)) {


      # Isolate the test
      sub.test <- x %>% dplyr::filter(TestId == tests[j])

      # Determine the node with the strongest RSSI value
      max.RSS <- sub.test[which.max(sub.test$avgRSS),]

      # Filter matrix of node distances by node with strongest avg.RSSI
      # and get all nodes within a particular distance of the node
      nodes.test <- dist.nodes_df %>%
        dplyr::filter(NodeId %in% max.RSS$node_id) %>%
        tidyr::gather(key = "NodeId", value = "distance", -NodeId) %>%
        dplyr::filter(distance <= dist.no)

      # Only keep nodes that are within the specified distance of node with strongest avg.RSSI
      sub.test.red <- sub.test %>%
        dplyr::filter(node_id %in% nodes.test$NodeId)

      # Calculate no nodes for the test
      no.nodes <- dplyr::n_distinct(sub.test.red$node_id)

      # If the number of nodes is not greater than 3 the rest of the loop will not be continued and the next
      # iteration of the loop is started
      if(no.nodes < 3) {
        next
      }

      # To deal with potential errors where the model fails due to bad starting values using tryCatch everything you want evaluated by tryCatch goes inside {},
      # then the error will be printed but the loop will continue

      # Non-linear test to optimize the location of unknown signal by looking at the radius around each Node based on estimated distance and the pairwise distance between all nodes
      tryCatch( {nls.test <- nls(e.dist ~ raster::pointDistance(data.frame(node_lng, node_lat), c(NodeUTMx_solution, NodeUTMy_solution), lonlat = T, allpairs = T),
                                 data = sub.test.red, start=list(NodeUTMx_solution=max.RSS$node_lng, NodeUTMy_solution=max.RSS$node_lat),
                                 control=nls.control(warnOnly = T, minFactor=1/30000, maxiter = 100)) # gives a warning, but doesn't stop the test from providing an estimate based on the last itteration before the warning


      # Determine an error around the point location estimate
      par.est = cbind(coef(nls.test), nlstools::confint2(nls.test))
      lng.ci.upper =  par.est[1,3]
      lng.ci.lower =  par.est[1,2]
      lat.ci.upper =  par.est[2,3]
      lat.ci.lower =  par.est[2,2] }

      ,error = function(e)  {cat("ERROR :",conditionMessage(e), "\n")})

      # estimated location of the point and error
      estimated.loc <- data.frame(TestId = tests[j], No.Nodes = no.nodes, x.est = par.est[1,1], y.est = par.est[2,1],
                                  x.ci.lower = lng.ci.lower, x.ci.upper = lng.ci.upper, y.ci.lower = lat.ci.lower, y.ci.upper = lat.ci.upper)

      # Populate dataframe with results
      estimated.location_results <- rbind(estimated.location_results, estimated.loc)

    }


    # combine estimated locations with true locations
    combined_results <- estimated.location_results %>%
      dplyr::left_join(test.UTM)

    # Calculate difference distance between estimated and true location
    dst <- raster::pointDistance(combined_results[,c("lon", "lat")], combined_results[,c(3,4)], lonlat = F, allpairs = F)

    # bring all together
    combined_results_final <- dplyr::bind_cols(combined_results, data.frame(diff.dist = dst)) %>%
      dplyr::mutate(filter = paste0("Distance", " ", DIST.FILTER[i]))

    # save file
    #write.csv(combined_results_final, paste0(outpath, "Trilateration.Test.Data_Filter.Distance.", DIST.FILTER[i], "_Results.csv"),  row.names = F)


    # Summary Stats of data for a given filter
    summary.stats <- combined_results_final %>%
      dplyr::summarise(n.est.tests = dplyr::n_distinct(TestId),
                       avg.no.nodes = mean(No.Nodes),
                       avg.diff = mean(diff.dist),
                       sd.diff = sd(diff.dist),
                       lower.ci = avg.diff - qnorm(0.975)*sd.diff/sqrt(n.est.tests),
                       upper.ci = avg.diff + qnorm(0.975)*sd.diff/sqrt(n.est.tests),
                       med.diff = median(diff.dist),
                       min.diff = min(diff.dist),
                       max.dist = max(diff.dist)) %>%
      dplyr::mutate(filter = paste0("Distance", " ", DIST.FILTER[i]))

    # populate empty dataframe
    summary.stats_results <- rbind(summary.stats_results, summary.stats)


  }

  # save file
  #write.csv(summary.stats_results, paste0(outpath, "Trilateration.Simulation_Filters.Distance_Summary.Stats.csv"),  row.names = F)

  return(list(combined_results_final, summary.stats_results))
}

prep.data <- function(x,y, SLIDE.TIME, GROUP.TIME, K, a, S, startval = NULL) {

  # supress warnings
  options(warn = -1)

  # Sliding window over RSSI values for a given TagId and NodeId
  # 1st argument: vector to iterate over and apply function
  # 2nd argument: datetime index to break into periods
  # 3rd argument: period to group by "hour", "minute", "second"
  # function to apply
  # the number of values before or after current element to include in sliding window
  beep.slide <- x %>%
    dplyr::group_by(tag_id, node_id) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(roll.TagRSSI = slider::slide_index_dbl(tag_rssi, time, mean,
                                                         .after = lubridate::seconds(SLIDE.TIME), .before = lubridate::seconds(SLIDE.TIME))) %>%
    ungroup()


  # Average RSSI values by each minute for each TagId and NodeId
  # ignore warning
  beep.grouped <- beep.slide %>%
    dplyr::group_by(tag_id)

  if (is.null(startval)) {
    beep.grouped <- beep.grouped %>%
    padr::thicken(GROUP.TIME, colname="Time.group", by = "time") %>%
    dplyr::group_by(tag_id, node_id, Time.group) %>%
    dplyr::summarise(mean_rssi = mean(roll.TagRSSI), beep_count = length(roll.TagRSSI)) %>%
    ungroup()} else {
      beep.grouped <- beep.grouped %>%
        padr::thicken(GROUP.TIME, colname="Time.group", by = "time", start_val=startval) %>%
        dplyr::group_by(tag_id, node_id, Time.group) %>%
        dplyr::summarise(mean_rssi = mean(roll.TagRSSI), beep_count = length(roll.TagRSSI)) %>%
        ungroup()
    }

  e.dist <- (log(beep.grouped$mean_rssi - K) - log(a)) / -S
  beep.grouped$e.dist <- e.dist

  # calculate radius around a node given the exponential relationship between RSSI and distance

  # Remove data with NAs produced from e.dist
  beep.grouped <- beep.grouped[complete.cases(beep.grouped),]

  # Change negative distances to 10 (RSSI values > intercept of exponential curve and thus negative) - indicates very close to the node
  beep.grouped <- beep.grouped %>%
    dplyr::mutate(e.dist = dplyr::case_when(e.dist < 0 ~ 10,
                                            e.dist >=0 ~ e.dist))

  # Add Node UTMs to data
  beep.grouped <- beep.grouped %>%
    dplyr::left_join(y[,c("node_id", "node_lng", "node_lat")])

  return(beep.grouped[order(beep.grouped$Time.group),])

}

trilateration <- function(x, nodes, RSS.FILTER) {

  # supress warnings
  options(warn = -1)


  # Identify the Nodes for the given dataset and filter nodes
  nodes.unique <- unique(x$node_id)
  nodes.red <- nodes %>% dplyr::filter(node_id %in% nodes.unique)

  # Calculate distance between nodes
  dist.nodes <- raster::pointDistance(nodes.red[,c("node_lng", "node_lat")], nodes.red[,c("node_lng", "node_lat")], lonlat = T, allpairs = T)

  # Make matrix into a dataframe with a row for NodeId
  dist.nodes_df <- data.frame(dist.nodes, row.names = nodes.red$node_id)
  colnames(dist.nodes_df) <- nodes.red$node_id
  dist.nodes_df$NodeId <- rownames(dist.nodes_df)

  # Create a vector of birds
  #beep.grouped$TagId <- as.factor(beep.grouped$tag_id)
  #beep.grouped$TagId <- droplevels(beep.grouped$TagId)
  bird <- unique(beep.grouped$tag_id)



  # Create a dataframe for output estimates
  estimated.location_results <- data.frame(TagId=character(), Time.group = POSIXct(), Hour = numeric(), No.Nodes = numeric(), UTMx_est=numeric(), UTMy_est=numeric(),
                                           x.LCI =numeric(), x.UCI =numeric(),  y.LCI = numeric(), y.UCI = numeric())

  # Loop through each bird
  for (k in 1:length(bird)) {


    # Filter data for the identified bird
    sub.bird <- beep.grouped %>% dplyr::filter(tag_id == bird[k])
    #sub.bird$TagId <- droplevels(sub.bird$TagId)

    # identify the unique bird and tests per bird
    #test.bird = unique(sub.bird$TagId)
    tests = unique(sub.bird$Time.group)

    # Indicate bird that is currently being processed and how many unique time periods to process
    #print(as.character(test.bird))
    print(length(tests))

    # Loop through unique time groups

    for(j in 1:length(tests)) {

      # Isolate the test
      sub.test <- sub.bird %>% dplyr::filter(Time.group == tests[j])

      # Determine the node with the strongest RSSI value
      max.RSSI <- sub.test[which.max(sub.test$mean_rssi),]

      # Filter matrix of node distances by node with strongest avg.RSSI
      # and get all nodes within a particular distance of that node
      nodes.test <- dist.nodes_df %>%
        dplyr::filter(NodeId %in% max.RSSI$node_id) %>%
        tidyr::gather(key = "NodeId", value = "distance", -NodeId) #%>%
        #dplyr::filter(distance <= DIST.filter)

      # Only keep nodes that are within the specified distance of node with strongest avg.RSSI
      sub.test.dist <- sub.test %>%
        dplyr::filter(node_id %in% nodes.test$NodeId)

      # Remove RSSI <= filter
      sub.test.dist.rssi <- sub.test.dist %>%
        dplyr::filter(mean_rssi >= RSS.filter)

      # Calculate no nodes for the test
      no.nodes <- dplyr::n_distinct(sub.test.dist.rssi$node_id)

      # Determine the hour of the observation
      test.hour <- lubridate::hour(max.RSSI$Time.group)

      # If the number of nodes is not greater than 3 the rest of the loop will not be continued and the next
      # iteration of the loop is started
      if(no.nodes < 3) {
        next
      }

      # To deal with potential errors where the model fails due to bad starting values using tryCatch everything you want evaluated by tryCatch goes inside {},
      # then the error will be printed but the loop will continue

      # Non-linear test to optimize the location of unknown signal by looking at the radius around each Node based on estimated distance and the pairwise distance between all nodes
      tryCatch( {nls.test <- nls(e.dist ~ raster::pointDistance(data.frame(node_lng, node_lat), c(UTMx_solution, UTMy_solution), lonlat = T, allpairs = T),
                                 data = sub.test.dist.rssi, start=list(UTMx_solution=max.RSSI$node_lng, UTMy_solution=max.RSSI$node_lat),
                                 control=nls.control(warnOnly = T, minFactor=1/30000, maxiter = 100)) # gives a warning, but doesn't stop the test from providing an estimate based on the last itteration before the warning


      # Determine an error around the point location estimate
      par.est = cbind(coef(nls.test), nlstools::confint2(nls.test))
      UTMx.ci.upper =  par.est[1,3]
      UTMx.ci.lower =  par.est[1,2]
      UTMy.ci.upper =  par.est[2,3]
      UTMy.ci.lower =  par.est[2,2] }

      ,error = function(e)  {cat("ERROR :",conditionMessage(e), "\n")})

      # estimated location of the point and error
      estimated.loc <- data.frame(TagId = bird[k], Time.group = tests[j], Hour = test.hour, No.Nodes = no.nodes, lon_est = par.est[1,1], lat_est = par.est[2,1],
                                  x.LCI = UTMx.ci.lower, x.UCI = UTMx.ci.upper,  y.LCI = UTMy.ci.lower, y.UCI = UTMy.ci.upper)


      # Populate dataframe with results
      estimated.location_results <- rbind(estimated.location_results, estimated.loc)

    }



    # save estimated locations
    #saveRDS(estimated.location_results, paste0(outpath, "Estimated.Locations_", START, "_", END, ".rds"))


  }

  return(estimated.location_results)

}
library(leaflet)

mapping <- function(node_locs, track_error_df, tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {
map <- leaflet() %>%
  addTiles(
    urlTemplate = tile_url,
    options = tileOptions(maxZoom = 25)
  ) %>%
  addCircleMarkers(
    data = node_locs,
    lat = node_locs$node_lat,
    lng = node_locs$node_lng,
    radius = 5,
    color = "cyan",
    fillColor = "cyan",
    fillOpacity = 0.5,
    label = node_locs$node_id
  )  %>%
  #addPolylines(
   # data = track_error_df,
   #lat = track_error_df$lat_est,
   # lng = track_error_df$lon_est,
    #color = "red",
  #  weight = 2
  #) %>%
  addCircleMarkers(
    data = track_error_df,
    lat = track_error_df$lat_est,
    lng = track_error_df$lon_est,
    radius = 1,
    color = "red",
    fillColor = "red",
    fillOpacity = 1.0)
    #label = paste(track_error_df$i, ":", as_datetime(track_error_df$time), " : ", track_error_df$error)
return(map)}
