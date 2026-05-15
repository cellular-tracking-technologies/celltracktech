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
    message(errtype)
    #print(filetype)
    message(y)
    contents <- db_prep(contents, filetype, sensor, y, begin)
    if(nrow(contents) < 1) {errtype <- 7}
    if (errtype < 7 & errtype != 2) {
      # z <- db_insert(contents, filetype, conn, y)
      z <- db_insert(contents=contents, filetype=filetype, conn=conn, y=y, begin=begin)
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
