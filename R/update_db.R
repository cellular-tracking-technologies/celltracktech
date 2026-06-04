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

  # ensure cumulative_solar_current can hold large values
  is_duckdb <- length(grep("duckdb", format(d))) > 0
  if (is_duckdb) {
    tryCatch(
      DBI::dbExecute(d, "ALTER TABLE node_health ALTER COLUMN cumulative_solar_current TYPE BIGINT"),
      error = function(e) NULL
    )
  } else {
    tryCatch(
      DBI::dbExecute(d, "ALTER TABLE node_health ALTER COLUMN cumulative_solar_current TYPE bigint"),
      error = function(e) NULL
    )
  }

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

  # check for any unzipped csv files
  myfiles1 = as.list(myfiles)
  for (i in 1:length(myfiles1)){
    if (i < length(myfiles1)) {

      regex_value = stringr::str_remove(myfiles1[[i+1]], '.gz')
      if(myfiles1[[i]] == regex_value) {
        myfiles1[[i]] <- NULL
        message(myfiles)
      }  else {
        next
      }
    }
  }

  myfiles = unlist(myfiles1)

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
