pop <- function(x) { # this was a function written before the data file table was added, no one should need this
  allnode <- DBI::dbReadTable(x, "node_health")
  allgps <- DBI::dbReadTable(x, "gps")
  allbeep <- DBI::dbReadTable(x, "raw")
  insertnew <- DBI::dbSendQuery(conn, paste("INSERT OR IGNORE INTO ", "data_file (path)", " VALUES ($)", sep = ""))
  DBI::dbBind(insertnew, params = list(unique(c(allnode$path, allgps$path, allbeep$path))))
  DBI::dbClearResult(insertnew)

  insertnew <- DBI::dbSendQuery(conn, paste("INSERT OR IGNORE INTO ", "nodes (node_id)", " VALUES ($)", sep = ""))
  DBI::dbBind(insertnew, params = list(unique(allnode$node_id)))
  DBI::dbClearResult(insertnew)
}
