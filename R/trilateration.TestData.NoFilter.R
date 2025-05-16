#' Trilateration - Test Data No Filter
#'
#' @param x - dataframe
#'
#' @returns list
#' @export
#'
#' @examples
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
