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
      table_fields <- DBI::dbListFields(conn, filetype)
      vars <- paste(table_fields[2:length(table_fields)],
                    sep = "",
                    collapse = ",")

      vals <- paste(seq_along(1:(length(table_fields) - 1)),
                    sep = "",
                    collapse = ", $")

      contents <- contents[, table_fields[2:length(table_fields)]] # need path and station_id columns

    } else {
      table_fields <- DBI::dbListFields(conn, filetype)
      vars <- paste(table_fields, sep = "", collapse = ",")
      vals <- paste(seq_along(1:length(table_fields)),
                    sep = "",
                    collapse = ", $")
      names(contents) <- tolower(names(contents))
      contents <- contents[, table_fields]
    }

    message(paste('db_insert:', nrow(contents), 'rows into', filetype))
    is_duckdb <- length(grep("duckdb", format(conn))) > 0
    h <- tryCatch({
      if (is_duckdb) {
        cols <- paste(colnames(contents), collapse = ", ")
        auto_id_tables <- c("raw", "blu", "node_raw", "node_blu")
        batch_size <- 10000L
        n <- nrow(contents)

        for (i in seq(1L, n, by = batch_size)) {
          end_idx <- min(i + batch_size - 1L, n)
          batch <- contents[i:end_idx, , drop = FALSE]

          # write batch to temp CSV, COPY into temp table
          tmp <- tempfile(fileext = ".csv")
          readr::write_csv(batch, tmp, na = "")
          temp_tbl <- paste0("temp_import_", filetype)
          DBI::dbExecute(conn, paste0(
            "CREATE OR REPLACE TEMPORARY TABLE ", temp_tbl,
            " AS SELECT ", cols, " FROM ", filetype, " LIMIT 0"))
          DBI::dbExecute(conn, paste0(
            "COPY ", temp_tbl, "(", cols, ") FROM '", tmp,
            "' (HEADER TRUE, AUTO_DETECT TRUE)"))
          unlink(tmp)

          # insert from temp into target
          if (filetype %in% auto_id_tables) {
            id_base <- DBI::dbGetQuery(conn, paste0(
              "SELECT COALESCE(MAX(id), 0) FROM ", filetype))[[1]]
            DBI::dbExecute(conn, paste0(
              "INSERT INTO ", filetype, "(id, ", cols, ") SELECT ",
              id_base, " + row_number() OVER () AS id, ", cols,
              " FROM ", temp_tbl))
          } else {
            DBI::dbExecute(conn, paste0(
              "INSERT INTO ", filetype, "(", cols, ") SELECT ", cols,
              " FROM ", temp_tbl, " ON CONFLICT DO NOTHING"))
          }

          DBI::dbExecute(conn, paste0("DROP TABLE ", temp_tbl))
          DBI::dbExecute(conn, "CHECKPOINT")
          message(paste('db_insert: batch', i, '-', end_idx, 'of', n, 'into', filetype))
        }
      } else {
        DBI::dbAppendTable(conn, filetype, contents)
      }
      query = paste("INSERT INTO ", "data_file (path)", " VALUES ($1) ON CONFLICT DO NOTHING", sep = "")
      insertnew <- DBI::dbSendQuery(conn, query)
      DBI::dbBind(insertnew, params = list(y))
      DBI::dbClearResult(insertnew)
      return(NULL)
    },
    error = function(err) {
      message(paste('db_insert error:', err))
      return(list(err, contents, y))
    }
    )
  }
  if (!exists("h")) {
    h <- NULL
  }
  return(h)
}
