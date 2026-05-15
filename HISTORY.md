# Changelog

## [2.0.0] - 2026-05-14

### Added

- `get_my_projects()`: new public API for listing user projects
- `get_station_file_list_paginated()`: date-based chunking for file list pagination with retry logic to avoid server timeouts
- `process_file()` / `process_directory()`: BLU payload parsing to extract battery voltage and temperature
- `create_outpath()`: directory structure creation for data downloads
- Single-line progress bar for file downloads showing download count, remaining files, and ETA
- Download summary table (data frame with filename and success/failed status)
- DuckDB support for node data import (`import_node_data`, `load_node_data`)
- Node SD card file import support
- GPS columns (hdop, vdop, pdop, on_time) added to database schema
- Sidekick calibration test data bundled with package
- Mapping and visualization functions (`map_*`, `plot_*`) added to package namespace
- Calculation functions (`calc_avg_activity`, `calc_rssi_v_dist`, etc.) added to package namespace

### Changed

- All function names converted to snake_case for consistency
- `get_data()`: replaced per-file progress output with single in-place progress bar
- `get_data()`: returns clean data frame of results instead of list of NULLs
- `get_my_data()`: removed noisy failed-file output, returns results from `get_data()`
- `download_files()`: removed per-file httr download progress bar
- `post()`: added `show_progress` parameter, request timing support, and `encoding = "UTF-8"` to suppress encoding warnings
- `db_insert()`: added `ON CONFLICT DO NOTHING` to fallback insert query to handle duplicate records gracefully
- `file_handle()`: added `show_col_types = FALSE` to all `readr::read_csv()` calls to suppress column spec messages
- `process_file()`: removed debug messages for file name and output directory
- `get_data()`: suppressed `as.integer()` coercion warning in filetype detection
- `get_data()`: early return with empty data frame when no new files to download
- `get_data()`: all messages from inner download loop suppressed to prevent progress bar flashing
- `newdb.R`: added `show_col_types = FALSE` to `read_csv()` calls
- `filecatch.R`: added `show_col_types = FALSE` to `read_csv()` call
- Database schema updated with node-specific tables and additional columns

### Fixed

- C stack overflow from `message()` calls on nested lists
- Accurate download count after filetype filtering
- File import error handling in `get_files_import()`
- Column renaming issues in `file_handle()`
- Unicode character filtering in node data import
- Duplicate record handling in GPS and node_health tables

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
