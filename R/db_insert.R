db_insert <- function(contents, filetype, conn, sensor=NA, y, begin=NULL) {
  if(any(colnames(contents) == "node_id")) {
    contents$node_id <- toupper(contents$node_id)
    if (length(which(!is.na(contents$node_id))) > 0) {
      nodeids <- contents$node_id[which(!is.na(contents$node_id))]
      insertnew <- DBI::dbSendQuery(conn,
                                    paste("INSERT INTO ",
                                    "nodes (node_id)",
                                    " VALUES ($1)
                                    ON CONFLICT DO NOTHING", sep = ""))

      DBI::dbBind(insertnew, params = list(unique(nodeids)))
      DBI::dbClearResult(insertnew)
    }
  } else {
    nodeids <- c()
  }
  if (filetype %in% c("raw",
                      "node_health",
                      "gps",
                      "blu",
                      'node_raw',
                      'node_health_from_node',
                      'node_gps',
                      'node_blu') & nrow(contents) > 0) {
    if (filetype %in% c("raw", "blu", 'node_raw', 'node_blu')) {
      vars <- paste(DBI::dbListFields(conn, filetype)[2:length(DBI::dbListFields(conn, filetype))],
                    sep = "",
                    collapse = ",")

      vals <- paste(seq_along(1:(length(DBI::dbListFields(conn, filetype)) - 1)),
                    sep = "",
                    collapse = ", $")

      contents <- contents[, DBI::dbListFields(conn, filetype)[2:length(DBI::dbListFields(conn, filetype))]] # need path and station_id columns
      contents

    } else {
      vars <- paste(DBI::dbListFields(conn, filetype), sep = "", collapse = ",")
      vals <- paste(seq_along(1:length(DBI::dbListFields(conn, filetype))),
                    sep = "",
                    collapse = ", $")
      names(contents) <- tolower(names(contents))
      contents <- contents[, DBI::dbListFields(conn, filetype)]
    }

    # browser()
    h <- tryCatch({
      tryCatch({
        DBI::dbWriteTable(conn, filetype, contents, append = TRUE)
        query = paste("INSERT INTO ", "data_file (path)", " VALUES ($1) ON CONFLICT DO NOTHING", sep = "")
        insertnew <-DBI::dbSendQuery(conn, query)
        # insertnew <- DBI::dbSendQuery(conn,
        #                               paste("INSERT INTO ",
        #                                     "data_file (path)",
        #                                     " VALUES ($1) ON CONFLICT DO NOTHING", sep = ""))  #CTT-FC16AD87C466-node-health.2022-07-15_104908.csv.gz
        DBI::dbBind(insertnew, params = list(y))
        DBI::dbClearResult(insertnew)
        return(NULL)
      },
      error = function(err) {
        # error handler picks up where error was generated, in Bob's script it breaks if header is missing
        # myquery <- paste("INSERT INTO ", filetype, " (", vars, ") VALUES ($", vals, ")
        # ON CONFLICT DO NOTHING", sep = "")
        myquery <- paste("INSERT INTO ", filetype, " (", vars, ") VALUES ($", vals, ")
                        ON CONFLICT DO NOTHING", sep = "")
        insertnew <- DBI::dbSendQuery(conn, myquery)
        DBI::dbBind(insertnew, params = unname(contents))
        DBI::dbClearResult(insertnew)
        insertnew <- DBI::dbSendQuery(conn, paste("INSERT INTO ", "data_file (path)", " VALUES ($1)
                                         ON CONFLICT DO NOTHING", sep = ""))  #CTT-FC16AD87C466-node-health.2022-07-15_104908.csv.gz
        DBI::dbBind(insertnew, params = list(y))
        DBI::dbClearResult(insertnew)
      })
    },
    error = function(err) {
      message(paste('h error', err))
      return(list(err, contents, y))
    }
    )
  }
  if (!exists("h")) {
    h <- NULL # h is boolean
  }
  # print(paste('what the hell is h???', h)) # h is boolean
  return(h)
}
