pop_proj <- function(a, conn) {
  b <- unname(as.data.frame(a))
  vars <- paste(DBI::dbListFields(conn, "ctt_project"), sep = "", collapse = ",")
  insertnew <- DBI::dbSendQuery(conn, paste("INSERT INTO ", "ctt_project", " (", vars, ") VALUES ($1, $2) ON CONFLICT DO NOTHING", sep = ""))
  # it is possible you should be using dbSendStatement for all of these
  DBI::dbBind(insertnew, params = b)
  DBI::dbClearResult(insertnew)

  basename <- a$name
  id <- a[["id"]]
  my_stations <- get_stations(project_id = id)
  message("RETURNED FROM API")
  message(my_stations)
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
  message("FORMATTED")
  message(mystations)

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
