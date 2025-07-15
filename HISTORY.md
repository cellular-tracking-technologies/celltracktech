# Changelog

## [1.0.1] - 2025-02-11

### Changed

- import_node_data(): Fix issue with importing node data into database. db_insert() had differing number of parameters, explicitly defined parameters in functions

- get_data(): trim project name to avoid creating directories with leading or trailing spaces. Windows does not create folders with trailing spaces, and if the project name has a trailing space, the downloaded files would not be saved there

## [1.0.0] - 2024-09-09

### Changed

- db_cleanup() deprecated: As of this version, rather than run this function to clean up your database records, please instead rebuild your database. Upon testing, the db_cleanup() function is taking longer than a fresh rebuild of a PostgreSQL database, and the operations are costly in terms of memory. To rebuild your database, please do the following...
- Close any open connection to your database (e.g. R, PGAdmin etc.)
- Run `dropdb <your database name>` in your terminal
- Create a new blank database with the desired name
- Run your modified version of the example script on the GitHub repository description
