library(celltracktech)
library(duckdb) # click no on pop up window
library(devtools) # needs RTools
library(readr)
library(dotenv)

### Install DuckDB from R Universe ###
# install.packages("duckdb", repos = c("https://duckdb.r-universe.dev", "https://cloud.r-project.org"))
# install.packages("https://github.com/duckdb/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", repos = NULL)


# Settings ----------------------------------------------------------------
load_dot_env(file='.env')

my_token <- Sys.getenv('API_KEY')
myproject <- "Meadows V2" #this is your project name on your CTT account
create_outpath(paste0('./examples/', myproject, '/'))
outpath <-'./examples/'

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "./examples/Meadows V2/meadows.duckdb",
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
  begin=as.Date("2023-08-01"),
  end=as.Date("2023-08-31"),
  filetypes=c("raw", "node_health")
)

update_db(con, outpath, myproject)
DBI::dbDisconnect(con)

time_elapse <- Sys.time() - start
print(time_elapse)

#raw <- tbl(con, "node_health") |> collect()


# Import Node Data --------------------------------------------------------
con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "./examples/Meadows V2/meadows.duckdb",
  read_only = FALSE
)

celltracktech::create_duck(con)

import_node_data(con,
                 outpath = outpath,
                 myproject="Meadows V2")

DBI::dbDisconnect(con)

# Database Functions ------------------------------------------------------

# list tables in database
DBI::dbListTables(con)

# list last 10 records in raw
raw = DBI::dbGetQuery(con, "SELECT * FROM raw "); raw
tail(raw)

# list last 10 records in blu
blu = DBI::dbGetQuery(con, "SELECT * FROM raw "); blu
tail(blu)
head(blu)


# list data in nodes table
node_table = DBI::dbGetQuery(con, 'SELECT * FROM nodes')

# list data in data_file table
df_table = DBI::dbGetQuery(con, 'SELECT * FROM data_file')


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


# Read Node CSV -----------------------------------------------------------

nodes = read_csv('~/Documents/cellular-tracking-technologies/data-analysis/celltracktech/vignettes/mouse_bird/Mouse Bird/nodes/B796B2/sample_1_beep.csv')
nodes
max(nodes$time)
max(raw_last10$time)

start <- min(nodes$time, na.rm=T)
end <- max(nodes$time, na.rm=T)
print(paste('start', start, 'end', end))
print(paste('time window query', paste0("select * from raw where time > '", start,"' and time < '",end,"'")))
test <- dbGetQuery(con, paste0("select * from raw where time > '", start,"' and time < '",end,"'"))
test

test$Time <- test$time
test$TagId <- test$tag_id
test$RadioId <- test$radio_id
test$NodeId <- test$node_id
test$TagRSSI <- test$tag_rssi
test$Validated <- test$validated
test$id <- as.character(test$id)

print(paste('test, from db, has station_id and path after indexing', test))
print(paste('df from file', df)) # df has capitalized columns
df <- dplyr::anti_join(nodes,test)
df$path = nodes$path
df$station_id = test$station_id[1]

distinct_stations = DBI::dbGetQuery(con,
                                    "SELECT DISTINCT station_id FROM raw ")
distinct_stations
