#' Update existing blu and node_blu database tables
#'
#' @param table_name string
#' @param payload dataframe column
#' @param id dataframe column
#'
#' @returns
#' @export
#'
#' @examples
update_existing_blu = function(table_name, con) {
  library(parallel)
  start_time = Sys.time()

  df = tbl(con, table_name) |>
    filter(is.na(battery_voltage_v) == TRUE) |>
    collect()
  df = df[1:2000,]
  # print(head(df))
  # alter table if columns do not exist
  dbSendQuery(con, paste0('ALTER TABLE ', table_name,
                          ' ADD COLUMN IF NOT EXISTS battery_voltage_v DECIMAL(6,3);
          ALTER TABLE ', table_name,
                          ' ADD COLUMN IF NOT EXISTS temperature_celsius DECIMAL(6,3)'))

  # create chunks
  chunk_size = 1000
  grouping_factor = cut(1:nrow(df), breaks = seq(0, nrow(df), by = chunk_size))
  chunks = split(df, grouping_factor)


  # print(clusterEvalQ(cl, "celltracktech" %in% .packages()))
  # print(clusterEvalQ(cl, exists("parseit")))

  for (i in chunks) {
    print(paste('Updating records', i$id[1], 'through', i$id[nrow(i)]))

    # get total number of rows in dataframe
    total = nrow(i)

    # create progress bar
    total <- 50
    pb <- txtProgressBar(min = 0, max = total, style = 3)

    # parse payload for each row and update database
    sapply(1:total, function(x) {
      try(
        db_exec(paste0(
          'UPDATE ', table_name,
          ' SET battery_voltage_v = ', parseit(i$payload[x])[[1]],
          ', temperature_celsius = ', parseit(i$payload[x])[[2]],
          ' WHERE id = ', i$id[x]
          ), con=con),
        silent = FALSE)
        Sys.sleep(0.1)
        setTxtProgressBar(pb, x)

        # db_exec(paste0('SELECT battery_voltage_v, temperature_celsius FROM ', table_name), con = con)
    })
  }

  end_time = Sys.time()
  diff = end_time - start_time
  print(paste('Database update took', diff, 'min long'))
}
