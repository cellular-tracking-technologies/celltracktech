# library(celltracktech)
# library(duckdb) # click no on pop up window
# library(devtools) # needs RTools
# library(readr)
# library(dotenv)

devtools::load_all()
library(duckplyr)
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
  dbname="meadows1"
)

# List Projects -----------------------------------------------------------

project_list(my_token)


# Get Data from CTT server ------------------------------------------------
get_my_data(
  my_token,
  outpath,
  # conn,
  myproject=myproject,
  begin=as.Date("2023-10-04"),
  end=as.Date("2023-10-16"),
  filetypes = c('raw')
)

# add downloaded data to database
update_db(conn, outpath, myproject)


# Create database from files on local computer ----------------------------

create_database(my_token = my_token,
                outpath = outpath,
                myproject = myproject,
                db_name = conn)

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
create_database(my_token = my_token,
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
raw = tbl(conn, 'raw') |> as_duckdb_tibble()
node_raw = tbl(conn, 'node_raw') |> as_duckdb_tibble()

blu = tbl(conn, 'blu') |> as_duckdb_tibble()
node_blu = tbl(conn, 'node_blu') |> as_duckdb_tibble()

node_blu = duckplyr::as_duckdb_tibble(DBI::dbGetQuery(conn, "SELECT * FROM node_blu"))
blu_after_insert = duckplyr::as_duckdb_tibble(DBI::dbGetQuery(conn, "SELECT * FROM blu"))

gps = duckplyr::as_duckdb_tibble(DBI::dbGetQuery(conn, "SELECT * FROM gps"))
gps = tbl(conn, 'gps') |> as_duckdb_tibble()

node_gps = duckplyr::as_duckdb_tibble(DBI::dbGetQuery(conn, "SELECT * FROM node_gps"))
gps_after_insert = duckplyr::as_duckdb_tibble(DBI::dbGetQuery(conn, "SELECT * FROM gps"))

node_health = tbl(conn, 'node_health') |> collect()

node_health_from_node = duckplyr::as_duckdb_tibble(DBI::dbGetQuery(conn, "SELECT * FROM node_health_from_node"))
node_health_after_insert = duckplyr::as_duckdb_tibble(DBI::dbGetQuery(conn, "SELECT * FROM node_health"))

unique_tags = tbl(conn, 'raw') |>
  group_by(tag_id) |>
  summarize(num_detect = n()) |>
  select(tag_id, num_detect) |>
  arrange(desc(num_detect)) |>
  as_duckdb_tibble()


raw = DBI::dbGetQuery(conn, 'SELECT * FROM raw ORDER BY time LIMIT 5')
head(raw)

unique_tags_sql= dbGetQuery(conn,
                            'SELECT tag_id, COUNT(*) AS num_detect
                            FROM raw
                            GROUP BY tag_id
                            ORDER BY num_detect DESC')

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

DBI::dbSendQuery(conn, 'ALTER TABLE node_raw ADD COLUMN radio_id smallint DEFAULT 4')

node_blu = DBI::dbGetQuery(conn, 'SELECT * FROM node_blu')

node_health_from_node = DBI::dbGetQuery(conn, 'SELECT * FROM node_health_from_node')

# list data in nodes table
node_table = DBI::dbGetQuery(conn, 'SELECT * FROM nodes')

# list data in data_file table
df_table = DBI::dbGetQuery(conn, 'SELECT * FROM data_file')

# change datatable type
dbGetQuery(conn, 'ALTER TABLE node_health ALTER sd_free TYPE NUMERIC(6,2)')

dbSendQuery(conn, 'ALTER TABLE node_health_from_node ALTER up_time SET DATA TYPE BIGINT')

node_gps = DBI::dbGetQuery(con, 'SELECT * FROM node_gps')

node_raw = DBI::dbGetQuery(con, 'SELECT * FROM node_raw')
DBI::dbSendQuery(con, 'ALTER TABLE node_raw ADD COLUMN radio_id smallint DEFAULT 4')
DBI::dbSendQuery(con, 'ALTER TABLE node_raw ADD COLUMN validated smallint')


data_types = dbGetQuery(conn, 'SELECT * FROM information_schema.columns')

raw_duplicates = raw_combine %>%
  group_by(time, tag_id, station_id, node_id) %>%
  filter(n() > 1)

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

# Look at the number of health records received from each node
node_record_counts <- node_health |>
  count(node_id)

# sort the node_record_counts by decreasing number
node_record_counts <- node_record_counts[order(node_record_counts$n, decreasing = TRUE),]

# plot the number of node health records based on node id
ggplot(node_record_counts,
       aes(x = factor(node_id,
                      node_record_counts$node_id),
           y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Health Record Count",
       y = "Node Id") +
  tag_hist_plot_theme()
