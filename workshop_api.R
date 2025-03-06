library(devtools)
install_github('cellular-tracking-technologies/celltracktech')

library(celltracktech)
library(duckdb) # added to package, need to re-install
library(dotenv)

# stores the time you run
start <- Sys.time()

# load env file into environment
load_dot_env(file='.env')


# install duckdb through R-universe
# install.packages("duckdb", repos = c("https://duckdb.r-universe.dev", "https://cloud.r-project.org"))
# renv::install('duckdb', repos = c('https://duckdb.r-universe.dev', 'https://cloud.r-project.org'))

# Settings ----------------------------------------------------------------
my_token <- Sys.getenv('WORKSHOP') # load env variable into my_token
myproject <- "Meadows V2" #this is your project name on your CTT account
outpath <- "./data/meadows/" #where your downloaded files are to go

# Create outpath folder if it does not exist
if (file.exists(outpath)) {
  print(paste('Folder exists, no need to create a new directory.'))
} else {
  # create a new sub directory inside the main path
  print(paste('Folder', outpath, 'does not exist, creating it now.'))
  dir.create(outpath)
}


# Connect to Database using DuckDB -----------------------------------------------------
con <- DBI::dbConnect(duckdb::duckdb(),
                      dbdir = "./data/meadows/meadows.duckdb",
                      read_only = FALSE)

# Get data from CTT Server ------------------------------------------------
get_my_data(my_token,
            outpath,
            con,
            myproject=myproject,
            begin=as.Date("2023-08-01"),
            end=as.Date("2023-08-02"),
            filetypes=c("raw", "node_health")
)


# Optional! Import Node data from SD Card ---------------------------------
import_node_data(d=con,
                 outpath=outpath,
                 myproject = myproject,
                 filetype=filetype)


# Update your local database with the recently downloaded data ------------
update_db(con, outpath, myproject)


# Disconnect from your database to save memory ----------------------------
DBI::dbDisconnect(con)

time_elapse <- Sys.time() - start
print(time_elapse)
