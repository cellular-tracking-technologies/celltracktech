# Changelog

## [1.0.0] - 2024-09-09

### Changed

- db_cleanup() deprecated: As of this version, rather than run this function to clean up your database records, please instead rebuild your database. Upon testing, the db_cleanup() function is taking longer than a fresh rebuild of a PostgreSQL database, and the operations are costly in terms of memory. To rebuild your database, please do the following...
- Close any open connection to your database (e.g. R, PGAdmin etc.)
- Run `dropdb <your database name>` in your terminal
- Create a new blank database with the desired name
- Run the following in your R script:
```
my_token <- "your API token"
outpath <- "where your files are or will be downloaded"
conn <- DBI::dbConnect(RPostgres::Postgres(), dbname="your database name")
get_my_data(my_token, outpath, conn=conn, myproject="your project name")
update_db(conn, outpath, myproject="your project name")
```