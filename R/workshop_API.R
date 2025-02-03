library(celltracktech)
library(duckdb) # click no on pop up window
library(devtools) # needs RTools
library(readr)

source('./R/api_postgres.R')
source('./R/filecatch.R')
source('./R/newdb.R')
source('./R/node.R')

start <- Sys.time()

### Install DuckDB from R Universe ###
# install.packages("duckdb", repos = c("https://duckdb.r-universe.dev", "https://cloud.r-project.org"))
# install.packages("https://github.com/duckdb/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", repos = NULL)

####SETTINGS#####
myproject <- "Meadows V2" #this is your project name on your CTT account
outpath <- "./vignettes/aos2024/" #where your downloaded files are to go
my_token <- "c2ed5f935e9b9d4c2e031f8a96277317b7502d989add5947656dbfbeee7082c5" # aos workshop token, has ctt office and meadows
# my_token <- 'd93101a4badb937259b244ab886ae9c7df33f2e7fb02573d6a5ccf7517d060c1' # account token 1, does not have any projects associated with it
# my_token <- '49a778c42209d6ab5c0145acc6c3850addd272fd373e8430c7dd40638843921e' # account token 2, no projects associated with it

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "./vignettes/aos2024/meadows.db",
  read_only = FALSE
)


# List Projects -----------------------------------------------------------

project_list(my_token)

################
get_my_data(
  my_token,
  outpath,
  con,
  myproject=myproject,
  begin=as.Date("2023-07-23"),
  end=as.Date("2023-07-25"),
  filetypes=c("raw", "node_health")
)

update_db(con, outpath, myproject)
DBI::dbDisconnect(con)

time_elapse <- Sys.time() - start
print(time_elapse)

#raw <- tbl(con, "node_health") |> collect()


# Import Node Data --------------------------------------------------------
source('./R/api_postgres.R')
source('./R/filecatch.R')
source('./R/newdb.R')
source('./R/node.R')

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "./vignettes/aos2024/meadows.db",
  read_only = FALSE
)

import_node_data(con,
                 outpath = outpath,
                 myproject="Meadows V2")

DBI::dbDisconnect(con)

# colnames(contents) = c('node_id', 'time', 'radio_id', 'tag_id', 'tag_rssi', 'validated')
# print(paste('colnames', tolower(colnames(contents))))
