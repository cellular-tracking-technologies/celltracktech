#' Update existing blu and node_blu database tables - using parallel
#'
#' @param table_name string
#' @param payload dataframe column
#' @param id dataframe column
#'
#' @returns
#' @export
#'
#' @examples
update_existing_parallel = function(table_name, con) {
  library(parallel)
  start_time = Sys.time()

  outpath <- "./examples/" # where your downloaded files are to go
  myproject = 'Black-throated finches in Australia'

  # con <- DBI::dbConnect(duckdb::duckdb(),
  #                       dbdir = paste0(outpath, myproject, '/', 'btfi.duckdb'),
  #                       read_only = FALSE)

  # df = tbl(con, table_name) |> collect()
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
  print(environment())


  # create cluster for parallel processing
  numCores = detectCores()
  cl <- makeCluster(4)

  clusterExport(cl, 'chunks', envir = environment())
  clusterEvalQ(cl, chunks)

  # clusterExport(cl, 'con', envir = environment())
  # clusterEvalQ(cl, con)
  # print(clusterEvalQ(cl, con))

  clusterExport(cl, 'table_name', envir = environment())
  clusterEvalQ(cl, table_name)

  # load packages into cluster
  clusterEvalQ(cl, {
    library(renv)
    # renv::install('cellular-tracking-technologies/celltracktech')
    renv::activate()
    library(celltracktech)
    library(devtools)
    library(parallel)
    library(R.utils)
    load_all()

    outpath <- "./examples/" # where your downloaded files are to go
    myproject = 'Black-throated finches in Australia'

    con <- DBI::dbConnect(duckdb::duckdb(),
                          dbdir = paste0(outpath, myproject, '/', 'btfi.duckdb'),
                          read_only = FALSE)

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

  })

  # print(clusterEvalQ(cl, "celltracktech" %in% .packages()))
  # print(clusterEvalQ(cl, exists("parseit")))

  for (i in chunks) {
    print(paste('Updating records', i$id[1], 'through', i$id[nrow(i)]))

    # get total number of rows in dataframe
    total = nrow(i)
    # print(i)
    print(parseit(i$payload[1])[[1]])
    print(parseit(i$payload[1])[[2]])

    print(DBI::dbListTables(con))

    parSapply(cl, 1:total, function(x) {
      print(parseit(i$payload[x])[[1]])
      print(parseit(i$payload[x])[[2]])
      try(
        db_exec(paste0(
          'UPDATE ', table_name,
          ' SET battery_voltage_v = ', parseit(i$payload[x])[[1]],
          ', temperature_celsius = ', parseit(i$payload[x])[[2]],
          ' WHERE id = ', i$id[x]
        ), con=con),
        silent = FALSE)
      Sys.sleep(0.1)

      db_exec(paste0('SELECT battery_voltage_v, temperature_celsius FROM ', table_name), con = con)
    })

  }
  stopCluster(cl)

  end_time = Sys.time()
  diff = end_time - start_time
  print(paste('Database update took', diff, 'min long'))
}
