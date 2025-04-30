#' Title
#'
#' @param x
#' @param nodes
#' @param RSS.FILTER
#'
#' @returns
#' @export
#'
#' @examples
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
