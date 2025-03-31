library(celltracktech)
library(duckdb) # click no on pop up window
library(devtools) # needs RTools
library(readr)
library(dotenv)

devtools::load_all()
### Install DuckDB from R Universe ###
# install.packages("duckdb", repos = c("https://duckdb.r-universe.dev", "https://cloud.r-project.org"))
# install.packages("https://github.com/duckdb/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", repos = NULL)


# Settings ----------------------------------------------------------------
load_dot_env(file='.env')

my_token <- Sys.getenv('API_KEY')
myproject <- "Meadows V2" #this is your project name on your CTT account
outpath <-'./examples/'
create_outpath(paste0(outpath, myproject, '/'))

# create duckdb database
conn <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "./examples/Meadows V2/meadows.duckdb",
  read_only = FALSE
)

# create Postgres database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname='meadows'
)

# List Projects -----------------------------------------------------------

project_list(my_token)

################
get_my_data(
  my_token,
  outpath,
  conn,
  myproject=myproject,
  begin=as.Date("2023-08-01"),
  end=as.Date("2023-08-03"),
  # filetypes=c("raw", "node_health")
  filetypes = c('raw', 'node_health','gps')
)

update_db(conn, outpath, myproject)
DBI::dbDisconnect(conn)

# Import Node Data --------------------------------------------------------
myproject <- "Meadows V2" #this is your project name on your CTT account
create_outpath(paste0('./examples/', myproject, '/'))
outpath <-'./examples/'

# add nodes folder to project folder
create_outpath(paste0(outpath, myproject, '/', 'nodes', '/'))

# add individual nodes to nodes folder (do this yourself)

# create database connection
conn <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "./examples/Meadows V2/meadows.duckdb",
  read_only = FALSE
)

# create database from nodes
create_node_db(my_token = my_token,
               outpath = outpath,
               myproject = myproject,
               db_name = conn)

# import node data

# if you get: Error in files_loc[1, ] : incorrect number of dimensions, you most likely don't have your individual node folders in the 'nodes' folder
import_node_data(conn,
                 outpath = outpath,
                 myproject="Meadows V2",
                 station_id = '6CA25D375881')

DBI::dbDisconnect(conn)

# Database Functions ------------------------------------------------------

# list tables in database
DBI::dbListTables(conn)

################### FIND A WAY TO GET THIS INFO INTO CREATE_NODE_DB!
# create projects list

myList = list()
myList$id = as.integer(73)
myList$name = 'Meadows V2'
super_list = list(myList)

ctt_project_station = dbGetQuery(conn, 'SELECT * FROM ctt_project_station')

ctt_project = dbGetQuery(conn, 'SELECT * FROM ctt_project')


# list last 10 records in raw
raw = DBI::dbGetQuery(conn, "SELECT * FROM raw") %>%
  filter(node_id == 'V3_NODE')
raw = DBI::dbGetQuery(conn, 'SELECT * FROM raw ORDER BY time LIMIT 5')
head(raw)

# list last 10 records in blu
blu = DBI::dbGetQuery(conn, "SELECT * FROM blu ")
blu = DBI::dbGetQuery(conn, 'SELECT * FROM blu ORDER BY time DESC LIMIT 5')

head(blu)
tail(blu)
blu05 = DBI::dbGetQuery(conn, 'SELECT * FROM blu LIMIT 5')
blu05

# using duckplyr
tail(blu)

# get gps records
gps = DBI::dbGetQuery(conn, 'SELECT * FROM gps')
gps

# get node_health records
node_health = DBI::dbGetQuery(conn,
                              'SELECT * FROM node_health ORDER BY blu_det DESC LIMIT 5')
node_health = DBI::dbGetQuery(conn, 'SELECT * FROM node_health')
# %>%
  filter(node_id != 'V3_NODE')
nh_v3 = node_health %>% filter(node_id == 'V3_NODE')

node_gps = DBI::dbGetQuery(conn, 'SELECT * FROM node_gps')

node_raw = DBI::dbGetQuery(conn, 'SELECT * FROM node_raw')

node_blu = DBI::dbGetQuery(conn, 'SELECT * FROM node_blu')

node_health_from_node = DBI::dbGetQuery(conn, 'SELECT * FROM node_health_from_node')


# list data in nodes table
node_table = DBI::dbGetQuery(conn, 'SELECT * FROM nodes')

# list data in data_file table
df_table = DBI::dbGetQuery(conn, 'SELECT * FROM data_file')

# change datatable type
dbGetQuery(conn, 'ALTER TABLE node_health ALTER sd_free TYPE NUMERIC(6,2)')

DBI::dbDisconnect(conn)


# Going from previous version to latest version of package ----------------

# if you are updating the celltracktech package to the latest version, gps and node_health will have additional columns in their respective tables. Newly uploaded files will populate those columns, while previously uploaded files will have NA in them. If you want to populate those old files, you will need to delete your database and repupload everything, or delete the contents in the 'data_file' table:

dbGetQuery(conn, 'DELETE FROM data_file')

# Remove corrupted data ---------------------------------------------------
e <- "./examples//Meadows V2/nodes/v3_node/health_0.csv"
conn = con
d = con

df_filtered = for (i in 1:nrow(df)) {
  gsub("[^\u0001-\u007F]+|<U\\+\\w+>","", i)
}

# Function to remove non-ASCII characters using iconv
remove_non_ascii <- function(x) {
  if (str_detect(x, "[^\\x00-\\x7F]") == TRUE) {
    }
  }

# Apply function to the text column
df$tag_id <- sapply(df$tag_id, remove_non_ascii)
df$time <- sapply(df$time, remove_non_ascii)
df$rssi <- sapply(df$rssi, remove_non_ascii)

print(df)
