#' Update existing blu and node_blu database tables
#' 
#' @description
#' Parses additional information from the blu series tags by updating an existing database. Will return the `battery_voltage_v` and `temperature_celsius` of the tag data as new fields in the connected database. 
#'
#' @param table_name string, either 'blu' or 'blu_node'
#' @param con the connection to your local database
#' @param payload dataframe column
#' @param id dataframe column
#'
#' @returns 
#' @export
#'
#' @examples
#' update_existing_blu('blu', con)
#' 
#' 
update_existing_blu = function(table_name, con) {
  start_time = Sys.time()

  df = tbl(con, table_name) |>
    # filter(is.na(battery_voltage_v) == TRUE) |>
    filter(if_any(matches("battery_voltage_v"), \(battery_voltage_v)
                  is.na(battery_voltage_v) == TRUE)) |> #check if column exists, then filter
    filter(LENGTH(as.character(payload)) == 8) |>
    collect()
  # print(head(df))
  # alter table if columns do not exist
  dbSendQuery(con,
              paste0('ALTER TABLE ', table_name,
                     ' ADD COLUMN IF NOT EXISTS battery_voltage_v DECIMAL(6,3);
                     ALTER TABLE ', table_name,
                     ' ADD COLUMN IF NOT EXISTS temperature_celsius DECIMAL(6,3)'
                     )
              )

  # create chunks
  chunk_size = 1000
  grouping_factor = cut(1:nrow(df), breaks = seq(0, nrow(df), by = chunk_size))
  chunks = split(df, grouping_factor)

  for (i in chunks) {
    cat(paste('\nUpdated records', i$id[1], 'through', i$id[nrow(i)], '\n'))

    # get total number of rows in dataframe
    total = nrow(i)

    # create progress bar
    pb <- txtProgressBar(min = 0, max = total, style = 3)

    # parse payload for each row and update database
    sapply(1:total, function(x) {
      tryCatch({
        # if (nchar(as.character(i$payload[x])) != 8) {
        #   next
        # }
        db_exec(paste0(
          'UPDATE ', table_name,
          ' SET battery_voltage_v = ', parseit(i$payload[x])[[1]],
          ', temperature_celsius = ', parseit(i$payload[x])[[2]],
          ' WHERE id = ', i$id[x]
          ),
          con=con)
        }, error = function(e) {
          message('Error processing id: ', i$id[x], '-', e$message)
          db_exec(paste0(
            'UPDATE ', table_name,
            ' SET battery_voltage_v = NULL',
            ', temperature_celsius = NULL',
            ' WHERE id = ', i$id[x]
          ), con=con)
        })
        Sys.sleep(0.1)
        setTxtProgressBar(pb, x)
    })
  }

  end_time = Sys.time()
  diff = end_time - start_time
  cat(paste('\nStart: ', start_time, '\nEnd: ', end_time, '\nDifference:', diff))
}
