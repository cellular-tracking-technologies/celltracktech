# celltracktech

R package for working with Cellular Tracking Technologies data!

To install this package, please run:

```         
library(devtools)
install_github('cellular-tracking-technologies/celltracktech')
```

An example script using the API tools to download files:

```         
library(celltracktech)
library(DBI)
start <- Sys.time()

####SETTINGS#####
my_token <- "your token here"
db_name <- "mydb"
myproject <- "CTT Project Name" #this is your project name on your CTT account
conn <- dbConnect(RPostgres::Postgres(), dbname=db_name)
################
outpath <- "~/Documents/data/radio_projects/myproject" #where your downloaded files are to go
get_my_data(my_token, "~/Documents/data/radio_projects/myproject", conn, myproject=myproject)
update_db(conn, outpath, myproject)
dbDisconnect(conn)

#findfiles(outpath, "directory path where you want your caught files to go")

time_elapse <- Sys.time() - start
print(time_elapse)
```

## Storing your Token in an .env file

If you want to sharing this repository with colleagues over GitHub, it is recommended NOT to paste your API token in the R script. The best way to store your API token is in an .env file.

First, create the .env file using the terminal:
![Screenshot of terminal window in R environment with code to create an .env file](./terminal_screenshot.jpg)
```
touch .env

echo "API_KEY=your_api_key" >> .env
```
Then use the "dotenv" library to load the token into your R environment.

```
install.packages('dotenv')
library(dotenv)

# load the env file
load_dot_env(file='.env')

# get your api key from the env file
my_token <- Sys.getenv('API_KEY')
```
