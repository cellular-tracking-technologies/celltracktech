# library(celltracktech)
# library(duckdb) # click no on pop up window
# library(devtools) # needs RTools
# library(readr)
# library(dotenv)
library(devtools)
### Install DuckDB from R Universe ###
# install.packages("duckdb", repos = c("https://duckdb.r-universe.dev", "https://cloud.r-project.org"))
# install.packages("https://github.com/duckdb/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", repos = NULL)


# Settings ----------------------------------------------------------------
# Load functions
# DEFS
source("aos_functions/defs/plot_themes.R")
# UTILS
source("aos_functions/functions/utils/get_time_value.R")
# NODE
source("aos_functions/functions/node/node_functions.R")
source('aos_functions/functions/node/node_functions.R')

load_dot_env(file='.env')

my_token <- Sys.getenv('BTFI')
db_name <- "btfi.duckdb"
myproject <- "Black-throated finches in Australia" #this is your project name on your CTT account
create_outpath('./examples/btfi')
outpath <-'./examples/'

# List Projects -----------------------------------------------------------

project_list(my_token)

# Create database connection ----------------------------------------------

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = paste0('./examples/', myproject, '/', 'btfi.duckdb'),
  read_only = FALSE
)

# Create database from local data -----------------------------------------

create_node_db(my_token, outpath, myproject, con)

# Import Node Data --------------------------------------------------------
import_node_data(con,
                 outpath = outpath,
                 myproject= myproject,
                 station_id = 'V3023D36B0FC')

update_db(con, outpath, myproject)

DBI::dbDisconnect(con)

node_health_from_node = tbl(con, 'node_health_from_node') |> as_duckdb_tibble()

# Plot Node Battery over Time ---------------------------------------------

# load node_health table into RStudio and only load the data between the set start and stop times
node_health_df <- tbl(con, "node_health") |>
  # filter(time >= start_time & time <= stop_time) |>
  collect()

# Look at the number of health records received from each node
node_record_counts <- node_health_df %>% count(node_id)

# sort the node_record_counts by decreasing number
node_record_counts <- node_record_counts[order(node_record_counts$n, decreasing = TRUE),]

# plot the number of node health records based on node id
ggplot(node_record_counts,
       aes(x=factor(node_id,node_record_counts$node_id),
           y=n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x="Health Record Count",y="Node Id") +
  tag_hist_plot_theme

# Plot the Battery voltage vs. time for all nodes
ggplot(node_health_df) +
  geom_point(aes(x=time,y=battery,colour = node_id)) +
  classic_plot_theme


# Plot the Battery voltage vs. time for all node_fjords
node_fords = node_health_df |>
  filter(node_id == 'NODE_FORDS') |>
  filter(year(time) > 2020)

ggplot(node_fords) +
  geom_point(aes(x=time,y=battery,colour = node_id)) +
  classic_plot_theme

# Plot the Battery & Solar voltage vs. time for a specific node
# Node 326710 is a normal working Node
selected_node_id <- "NODE_FORDS"
batt_solar_plot <- plot_battery_solar(node_health_df = node_health_df, selected_node_id = selected_node_id)
batt_solar_plot

# based on the Battery voltage vs. time plot, Node 32909D has a low battery voltage
selected_node_id <- '32909D'
batt_solar_plot <- plot_battery_solar(node_health_df = node_health_df, selected_node_id = selected_node_id)
batt_solar_plot

# Database Functions ------------------------------------------------------

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = paste0('./examples/', myproject, '/', 'btfi.duckdb'),
  read_only = FALSE
)

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
gps_distinct = gps %>% distinct()

node_health = DBI::dbGetQuery(con, 'SELECT * FROM node_health')
node_health_distinct = node_health %>% filter(time == "2025-01-09 20:49:27 UTC")

head(node_health)

node_health7 = node_health %>%
  filter(node_id == 'NODE_7')

node_health6 = node_health %>%
  filter(node_id == 'NODE_6')

# list data in nodes table
node_table = DBI::dbGetQuery(con, 'SELECT * FROM nodes')

# list data in data_file table
df_table = DBI::dbGetQuery(con, 'SELECT * FROM data_file')

DBI::dbDisconnect(con)


