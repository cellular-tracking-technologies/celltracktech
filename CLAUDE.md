# celltracktech R Package

## On startup

Always read `summary.md` at the project root first to understand the full R package structure and all available functions before working on any task.

## Overview

This is an R package for downloading, processing, analyzing, and visualizing wildlife tracking data from Cellular Tracking Technologies (CTT) sensor stations. It supports PostgreSQL and DuckDB backends.

## Key entry points

- **API downloads**: `get_my_data()` is the main user-facing download function; `get_data()` orchestrates per-project workflows
- **Database setup**: `create_database()` creates and populates a new database
- **Localization**: `calculate_track()` performs grid search localization; `trilateration()` does multilateration
- **Visualization**: `map_*()` functions use leaflet; `plot_*()` functions use ggplot2
