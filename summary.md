# celltracktech R Package — Function Summary

This package provides tools for downloading, processing, analyzing, and visualizing wildlife tracking data from Cellular Tracking Technologies (CTT) sensor stations. It supports both PostgreSQL and DuckDB backends for data storage.

---

## API & Data Fetching

### `get_my_projects()` — get_my_projects.R
Retrieves the list of project names associated with the user's API account. Returns a character vector of project names.

### `get_stations()` — get_stations.R
Fetches all sensor stations associated with a specific project from the API via POST request with the project ID.

### `get_station_file_list()` — get_station_file_list.R
Retrieves file metadata for a given station within an optional date range, with optional file type filtering and pagination parameters.

### `get_station_file_list_paginated()` — get_station_file_list_paginated.R
Automatically paginates through station files using date-based chunks to avoid server timeouts. Configurable chunk size and retry logic for failed requests.

### `download_files()` — download_files.R
Downloads a specific file by ID from the API, with error handling that attempts to bypass encoding issues on failure.

### `get_my_data()` — get_my_data.R
Main function to download sensor station data with optional direct import to a local database (PostgreSQL or DuckDB). Supports filtering by station and date range.

### `get_data()` — get_data.R
Orchestrates the download workflow for a single project: retrieves file lists, compares against local files, downloads new files, and optionally inserts them into the database.

### `post()` — post.R
Generic HTTP POST wrapper for API requests. Adds authentication token, handles timeouts, and checks for server errors.

### `project_list()` — project_list.R
Retrieves and filters the user's projects from the API, with error handling for invalid project names.

### `get_files_import()` — get_files_import.R
Handles importing a single downloaded file: validates, processes, and inserts into the database with error tracking.

---

## Data Loading & Preparation

### `load_csv()` — load_csv.R
Loads CSV or CSV.GZ files from disk into a dataframe, automatically detecting and handling gzip compression.

### `load_dot_env()` — load_dot_env.R
Re-exported wrapper from the dotenv package to load environment variables from a .env file.

### `load_node_detection_data()` — load_node_detection_data.R
Loads and processes node detection CSV files within a specified time range. Removes duplicates and malformed records, returns combined dataframe.

### `load_node_health_files()` — load_node_health.R
Loads and processes node health CSV files, validates latitude/longitude bounds, and returns cleaned dataframe with timing.

### `load_sidekick_data()` — load_sidekick_data.R
Loads sidekick calibration data CSV and standardizes column names based on the number of columns present.

### `load_node_data()` — newdb.R
Loads and processes node-collected data files (beep, GPS, health) with format detection and database insertion, including payload parsing for BLU tags.

### `data.setup()` / `data_setup()` — data.setup.R
Combines sidekick calibration data with tag detection data, groups by test location/time, and calculates distances to nodes.

### `prep.data()` / `prep_data()` — prep.data.R
Prepares detection data by applying sliding window and time-based grouping to RSSI values. Estimates distance from RSSI using exponential model and adds node coordinates.

---

## Data Validation & Processing

### `badrow()` — badrow.R
Identifies and attempts to fix malformed CSV rows by detecting column count mismatches and restoring corrupted records.

### `goodrows()` — goodrows.R
Wrapper function to fix multiple malformed rows using the fixrow function.

### `file_handle()` — file_handle.R
Comprehensive file validation and processing: reads CSV, fixes column names, handles malformed rows, validates timestamps, and filters by node ID length.

### `error_files()` — error_files.R
Identifies files with errors and copies them to separate directories organized by error type (missing header, empty, row issues).

### `findfiles()` — filecatch.R
Scans directory for files with errors and attempts to read them, separating problematic files into error directory.

### `get_file_info()` — get_file_info.R
Parses filename to extract filetype (raw, node_health, gps, blu), sensor ID, and returns metadata as a list.

### `db_prep()` — db_prep.R
Prepares data for database insertion: validates timestamps, standardizes column names, removes invalid records, filters by ID length and duplicates.

### `db_insert()` — db_insert.R
Inserts validated data into the appropriate database table. Handles parameterized queries and tracks file paths.

---

## Database Management

### `create_db()` — create_db.R
Creates a blank PostgreSQL database schema with all necessary tables (raw, blu, node_health, gps, etc.) with proper constraints and relationships.

### `create_duck()` — create_duck.R
Creates a blank DuckDB database schema matching PostgreSQL structure, using sequences for auto-incrementing IDs.

### `create_database()` — create_database.R
High-level function to create and populate a database (PostgreSQL or DuckDB) with project and station data from the API.

### `pop_proj()` — pop_proj.R
Populates ctt_project and ctt_project_station tables with project metadata retrieved from the API.

### `pop()` — pop.R
Legacy function (deprecated) to populate data_file and nodes tables.

### `db_cleanup()` — newdb.R
Removes duplicate and invalid records from raw, node_health, and nodes tables. Ensures all files are tracked in data_file table.

### `import_node_data()` — newdb.R
Imports beep/GPS/health files from a local nodes folder and inserts them into the database.

### `update_db()` — update_db.R
Main function to update database with newly downloaded files: checks for duplicates and imports missing files.

### `patch()` — patch.R
Identifies corrupted files and removes bad records from the database.

### `update_existing_blu()` — update_existing_blu.R
Parses BLU payload data and updates battery voltage and temperature fields in blu/node_blu tables for incomplete records.

### `update_existing_parallel()` — update_existing_parallel.R
Parallel processing version of update_existing_blu using multi-core computation for faster payload parsing and database updates.

---

## File & Data Utilities

### `process_file()` — process_file.R
Loads a CSV/CSV.GZ file, parses BLU payload to extract battery voltage and temperature, and saves enhanced CSV.

### `process_directory()` — process_directory.R
Batch processes all CSV/CSV.GZ files in a directory by applying payload parsing and generating output files.

### `parseit()` — parseit.R
Parses hexadecimal payload strings from BLU tags to extract battery voltage (millivolts) and temperature (millidegrees). Returns values in volts and Celsius.

### `create_outpath()` — create_outpath.R
Creates directory structure for data downloads, handling nested paths and existing directories gracefully.

### `querygen()` — querygen.R
Generates SQL WHERE clause from a dataframe row, converting NA values to SQL NULL syntax.

### `correct_colnames()` — api_utils.R
Fixes column names by removing X prefixes and converting encoded timestamps. Used internally for parsing malformed CSV headers.

### `fixtime()` — api_utils.R
Converts datetime strings with ISO 8601 or space-separated format to POSIXct objects in UTC timezone.

### `fixrow()` — api_utils.R
Repairs a single malformed CSV row by extracting timestamp and converting data types appropriately.

### `resave()` — api_utils.R
Saves variables to an RData file while preserving existing variables in that file.

### `is_posixct()` — api_utils.R
Type checker to determine if an object is a POSIXct datetime.

### `camel_to_snake()` — camel_to_snake.R
Converts camelCase strings to snake_case format for column name standardization.

### `get_time_value()` — get_time_value.R
Converts time string to Unix timestamp integer for storage and comparison.

### `timecheck()` — timecheck.R
Validates and converts time values, handling both POSIXct and character formats.

### `timeset()` — timeset.R
Adds "UTC" timezone label to time values for display purposes.

### `out()` — out.R
Extracts and parses a time column from a dataframe, fixing malformed timestamps.

### `node_file()` — node_file.R
Aggregates node health data to calculate median coordinates per node, used for fixed node location estimation.

### `export_node_locations()` — export_node_locations.R
Exports node locations to CSV file with standard deviations.

---

## Calculations & Analysis

### `haversine()` — haversine.R
Calculates great circle distance between two geographic points in meters using the Haversine formula.

### `predict_rssi()` — rssi_v_dist.R
Predicts RSSI value from distance using exponential model coefficients (`a - b * exp(-c * distance)`).

### `predict_dist()` — rssi_v_dist.R
Estimates distance from RSSI value using inverse exponential relationship. Returns 1000 m if RSSI exceeds model asymptote.

### `calculate_node_locations()` — calculate_node_locations.R
Aggregates node health data to calculate mean and standard deviation of latitude/longitude for each node.

### `calculate_tag_activity()` — calculate_tag_activity.R
Calculates tag activity as RSSI changes between consecutive detections within defined time windows.

### `calc_avg_activity()` — calc_avg_activity.R
Aggregates tag activity into hourly bins with mean and standard deviation calculations.

### `calc_rssi_v_dist()` — calc_rssi_v_dist.R
Generates RSSI vs. distance calibration dataset by matching sidekick detections with node-detected beeps and calculating distances.

### `calc_receiver_values()` — calc_receiver_values.R
For a given timestamp, calculates average and filtered RSSI for each node with active detections. Applies low-pass filtering and time offsets.

### `calc_grid_values()` — calc_grid_values.R
Calculates likelihood value for each grid cell based on RSSI discrepancies between observed and expected values across all receivers.

### `calc_location_density()` — calc_location_density.R
Counts estimated tag locations falling within each grid cell to create location density heatmap.

### `calc_track_error()` — calc_track_error.R
Compares estimated track positions against sidekick ground truth locations, calculating error distances.

### `calculate_track()` — calc_track.R
Performs grid search localization over time: evaluates receiver positions, fits multilateration model, and produces time-series track of estimated positions.

### `estimate.distance()` / `estimate_distance()` — estimate.distance.R
Converts RSSI-derived distances to estimated distances using calibrated exponential model. Handles negative distances by setting to 10 m.

### `trilateration()` — trilateration.R
Performs multilateration localization using NLS fitting with node distances and RSSI values to estimate tag position and confidence intervals.

### `trilateration.TestData.NoFilter()` — trilateration.TestData.NoFilter.R
Evaluates trilateration accuracy with no filtering. Computes localization error compared to true positions.

### `trilateration.TestData.RSS.Filter()` — trilateration.TestData.RSS.Filter.R
Tests trilateration with multiple RSSI threshold filters. Reports accuracy metrics and summary statistics for each filter level.

### `trilateration.TestData.Distance.Filter()` — trilateration.TestData.Distance.Filter.R
Tests trilateration with multiple node distance filters. Reports accuracy metrics for each distance threshold.

### `get_tag_detection_count()` — get_tag_detections.R
Filters and counts detections per tag ID. Returns tags exceeding minimum detection threshold.

### `detection_summary()` — detection_summary.R
Generates summary statistics for each tag: detection count, first/last detection times, and duration.

---

## Grid & Spatial

### `build_grid()` — build_grid.R
Constructs a regular grid of specified resolution around node locations for grid search localization.

---

## Mapping & Visualization

### `map_node_locations()` — map_node_locations.R
Creates leaflet map showing node locations as markers with node IDs.

### `map_track()` — map_track.R
Visualizes track as polyline connecting positions over time with node locations and timestamps.

### `map_track_error()` — map_track_error.R
Compares estimated track against sidekick ground truth: shows both as colored polylines with error information.

### `map_calibration_track()` — map_calibration_track.R
Maps calibration track showing sidekick path and node locations.

### `map_grid()` / `draw_grid()` — map_grid.R
Draws grid cells on map with node locations, useful for visualizing search grid.

### `map_grid_solution()` / `map_single_solution()` — map_grid_solution.R
Visualizes grid search solution with value heatmap, showing best-estimate position and nodes with detections.

### `map_latest_solution()` — map_grid_solution.R
Extended version of map_single_solution that includes track history on the same map.

### `map_location_density()` — map_location_density.R
Creates heatmap showing density of estimated positions within grid cells.

### `mapping()` — mapping.R
General mapping function showing nodes and estimated positions with error information.

### `draw_single_solution()` — draw_single_solution.R
Creates detailed map with grid heatmap, receiver positions, and grid search solution.

### `map_multilat()` — compare_w_trilat.R
Compares grid search and multilateration solutions against sidekick ground truth, showing all tracks with error lines.

### `plot_node_locations()` — plot_node_locations.R
Generates ggplot scatter plot showing node mean locations and individual health observations.

### `plot_battery_solar()` — plot_battery_solar.R
Time-series plot of battery and solar voltage for a selected node.

### `plot_calibration_result()` — plot_calibration_result.R
Scatter plot of RSSI vs. distance with fitted exponential curve, colored by node ID.

### `classic_plot_theme()` — classic_plot_theme.R
Returns ggplot2 theme with black axes, grid, white background, and specific font sizes for publication-quality plots.

### `tag_hist_plot_theme()` — tag_hist_plot_theme.R
Returns ggplot2 theme optimized for histograms with Courier font for axis labels.

---

## Data Documentation

### `data.R`
Documents bundled package datasets including `node_health` (node GPS/health records) and `detections` (tag detection records with RSSI values). These are used for examples and testing.
