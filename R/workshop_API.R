library(celltracktech)
library(duckdb) # click no on pop up window
library(devtools) # needs RTools
# source('./api_postgres.R')
# source('./filecatch.R')
# source('./newdb.R')
# source('./node.R')

start <- Sys.time()

### Install DuckDB from R Universe ###
# install.packages("duckdb", repos = c("https://duckdb.r-universe.dev", "https://cloud.r-project.org"))

####SETTINGS#####
myproject <- "Meadows V2" #this is your project name on your CTT account
outpath <- "../vignettes/aos2024/" #where your downloaded files are to go
my_token <- "c2ed5f935e9b9d4c2e031f8a96277317b7502d989add5947656dbfbeee7082c5"
# my_token <- '49a778c42209d6ab5c0145acc6c3850addd272fd373e8430c7dd40638843921e'

con <- DBI::dbConnect(
  duckdb::duckdb(), 
  # dbdir = '../vignettes/aos2024',
  # dbdir = './',
  read_only = FALSE
)

################
get_my_data(
  my_token, 
  outpath, 
  con, 
  myproject=myproject, 
  begin=as.Date("2024-08-01"), 
  end=as.Date("2024-08-03"), 
  filetypes=c("raw", "node_health")
)

update_db(con, outpath, myproject)
DBI::dbDisconnect(con)

time_elapse <- Sys.time() - start
print(time_elapse)

#raw <- tbl(con, "node_health") |> collect()
