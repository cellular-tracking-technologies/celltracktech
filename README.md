# celltracktech
R package for working with Cellular Tracking Technologies data!  

To install this package, please run:  
```
library(devtools)
install_github('cellular-tracking-technologies/celltracktech')``
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
