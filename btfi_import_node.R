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

my_token <- Sys.getenv('BTFI')
db_name <- "btfi.duckdb"
myproject <- "Black-throated finches in Australia" #this is your project name on your CTT account
create_outpath('./examples/btfi')
outpath <-'./examples/btfi/'

# List Projects -----------------------------------------------------------

project_list(my_token)

# Create database connection ----------------------------------------------

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "./examples/btfi/btfi5.duckdb",
  read_only = FALSE
)

# Create database from local data -----------------------------------------

create_duck(con)

# Import Node Data --------------------------------------------------------
import_node_data(con,
                 outpath = outpath,
                 myproject=myproject)

update_db(con, outpath, myproject)

DBI::dbDisconnect(con)

# Database Functions ------------------------------------------------------

# list tables in database
DBI::dbListTables(con)

# list last 10 records in raw
raw = DBI::dbGetQuery(con, "SELECT * FROM raw ")
head(raw)

# list last 10 records in blu
blu = DBI::dbGetQuery(con, "SELECT * FROM blu")
head(blu)

# get gps records
gps = DBI::dbGetQuery(con, 'SELECT * FROM gps')
head(gps)

node_health = DBI::dbGetQuery(con, 'SELECT * FROM node_health')
head(node_health)

# list data in nodes table
node_table = DBI::dbGetQuery(con, 'SELECT * FROM nodes')

# list data in data_file table
df_table = DBI::dbGetQuery(con, 'SELECT * FROM data_file')


