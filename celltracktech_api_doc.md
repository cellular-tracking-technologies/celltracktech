# celltracktech API Documentation

All endpoints use the CTT Internet of Wildlife REST API.

- **Base URL:** `https://api.internetofwildlife.com`
- **Protocol:** HTTPS
- **Method:** All endpoints use `POST`
- **Encoding:** JSON (`encode = "json"`)
- **Timeout:** 60 seconds per request
- **Authentication:** API token passed in the JSON body as `token`

Token is loaded from a `.env` file via `dotenv` and accessed with `Sys.getenv("MY_TOKEN")`.

---

## Endpoints

### 1. List Projects

**Path:** `/station/api/projects`

Retrieves all projects associated with the authenticated user's account.

**Request Body:**

| Field   | Type   | Required | Description              |
|---------|--------|----------|--------------------------|
| `token` | string | Yes      | API authentication token |

**curl example:**

```bash
curl -X POST https://api.internetofwildlife.com/station/api/projects \
  -H "Content-Type: application/json" \
  -d '{"token": "YOUR_API_TOKEN"}'
```

**Response:**

```json
{
  "projects": [
    {
      "id": "<project-id>",
      "name": "<project-name>",
      ...
    }
  ]
}
```

**Errors:**

| Error | Cause | What You See |
|-------|-------|--------------|
| HTTP 401 Unauthorized | Invalid or expired API token | `httr::stop_for_status()` raises an error: `"Unauthorized (HTTP 401)"` |
| HTTP 503 Service Unavailable | Server overloaded or temporarily down | Warning: `"Server query timeout — try narrowing your date range or using pagination"`, then execution stops |
| Connection timeout | Network issue or server unreachable | R error: `"Timeout was reached"` after 60 seconds |
| Empty/NULL token | `.env` file missing or `MY_TOKEN` not set | The API returns an authentication error; `Sys.getenv("MY_TOKEN")` silently returns `""` if unset |
| Project not found | Token is valid but the specified `myproject` name doesn't match any project | `project_list()` prints: `"The project you entered is not found in your project list. Check your spelling and if you have access to the project."` |

**Used by:**
- `get_my_projects()` in `R/get_my_projects.R` — returns a character vector of project names
- `project_list()` in `R/project_list.R` — returns filtered project list with optional name matching

---

### 2. List Stations

**Path:** `/station/api/stations/`

Retrieves all sensor stations deployed under a given project.

**Request Body:**

| Field        | Type   | Required | Description        |
|--------------|--------|----------|--------------------|
| `token`      | string | Yes      | API auth token     |
| `project-id` | string | Yes     | Project identifier |

**curl example:**

```bash
curl -X POST https://api.internetofwildlife.com/station/api/stations/ \
  -H "Content-Type: application/json" \
  -d '{"token": "YOUR_API_TOKEN", "project-id": "YOUR_PROJECT_ID"}'
```

**Response:**

```json
{
  "stations": [
    {
      "station": {
        "id": "<station-id>",
        ...
      },
      "deploy-at": "2024-01-15T00:00:00.000Z",
      "end-at": "2024-06-01T00:00:00.000Z"
    }
  ]
}
```

Each station entry includes:
- `station.id` — unique station identifier
- `deploy-at` — ISO 8601 deployment start timestamp
- `end-at` — ISO 8601 deployment end timestamp (may be null for active stations)

**Errors:**

| Error | Cause | What You See |
|-------|-------|--------------|
| HTTP 401 Unauthorized | Invalid or expired API token | `httr::stop_for_status()` raises an error: `"Unauthorized (HTTP 401)"` |
| HTTP 503 Service Unavailable | Server overloaded or temporarily down | Warning: `"Server query timeout — try narrowing your date range or using pagination"`, then execution stops |
| Connection timeout | Network issue or server unreachable | R error: `"Timeout was reached"` after 60 seconds |
| Invalid project ID | `project-id` does not exist or user lacks access | The API returns an error status; `httr::stop_for_status()` stops execution |
| Empty station list | Project exists but has no deployed stations | Response returns `"stations": []`; downstream code in `get_data()` skips processing |

**Used by:**
- `get_stations()` in `R/get_stations.R`
- `get_data()` in `R/get_data.R` — iterates stations to build file lists

---

### 3. List Station Files

**Path:** `/station/api/file-list`

Retrieves metadata for data files available from a specific station, with optional date range and file type filtering. Supports pagination via `limit` and `offset`.

**Request Body:**

| Field        | Type     | Required | Description                                  |
|--------------|----------|----------|----------------------------------------------|
| `token`      | string   | Yes      | API auth token                               |
| `station-id` | string  | Yes      | Station identifier                           |
| `begin`      | date     | Yes      | Start date for file search (ISO date)        |
| `end`        | date     | No       | End date for file search (ISO date)          |
| `file-types` | string[] | No      | Filter by file types (see valid types below) |
| `limit`      | integer  | No       | Max number of files to return (pagination)   |
| `offset`     | integer  | No       | Number of files to skip (pagination)         |

**Valid file types:** `data`, `node-data`, `gps`, `log`, `telemetry`, `sensorgnome`, `ble`, `blu`

**curl examples:**

Basic request with required fields:

```bash
curl -X POST https://api.internetofwildlife.com/station/api/file-list \
  -H "Content-Type: application/json" \
  -d '{"token": "YOUR_API_TOKEN", "station-id": "YOUR_STATION_ID", "begin": "2024-01-01"}'
```

With date range, file type filter, and pagination:

```bash
curl -X POST https://api.internetofwildlife.com/station/api/file-list \
  -H "Content-Type: application/json" \
  -d '{
    "token": "YOUR_API_TOKEN",
    "station-id": "YOUR_STATION_ID",
    "begin": "2024-01-01",
    "end": "2024-02-01",
    "file-types": ["data", "gps"],
    "limit": 100,
    "offset": 0
  }'
```

**Response:**

```json
{
  "files": [
    {
      "id": "<file-id>",
      "name": "<filename>",
      ...
    }
  ]
}
```

**Used by:**
- `get_station_file_list()` in `R/get_station_file_list.R` — single request with optional pagination params
- `get_station_file_list_paginated()` in `R/get_station_file_list_paginated.R` — automatically chunks requests into date windows (default 30 days) with retry logic for 503 timeouts

**Errors:**

| Error | Cause | What You See |
|-------|-------|--------------|
| HTTP 401 Unauthorized | Invalid or expired API token | `httr::stop_for_status()` raises an error: `"Unauthorized (HTTP 401)"` |
| HTTP 503 Service Unavailable | Date range too large; server times out processing the query | Warning: `"Server query timeout — try narrowing your date range or using pagination"`, then execution stops. The paginated wrapper catches this and retries with message: `"Request timed out, retrying (attempt X of Y)..."` |
| Connection timeout | Network issue or server unreachable | R error: `"Timeout was reached"` after 60 seconds. The paginated wrapper retries up to `max_retries` times (default 3) with exponential backoff (2s, 4s, 6s) |
| Invalid file type | A value in `file-types` is not one of the 8 valid types | Warning: `"WARNING: invalid file type specified - ignoring: <type>"`. The invalid type is silently dropped and the request proceeds with only the valid types |
| Invalid station ID | `station-id` does not exist | The API returns an error status; `httr::stop_for_status()` stops execution |
| No files in range | Station has no files for the given date range | Response returns `"files": []`; `get_data()` prints `"no files found from API"` and returns an empty list |
| Max retries exceeded | Paginated wrapper exhausts all retry attempts on a chunk | The original error is re-thrown via `stop(e)`, halting execution |

**Notes:**
- Large date ranges may cause 503 server timeouts. Use `get_station_file_list_paginated()` to handle this automatically.
- The paginated version retries failed chunks up to `max_retries` times (default 3) with exponential backoff.

---

### 4. Download File

**Path:** `/station/api/download-file/`

Downloads the contents of a single data file by its ID.

**Request Body:**

| Field             | Type   | Required | Description                                       |
|-------------------|--------|----------|---------------------------------------------------|
| `token`           | string | Yes      | API auth token                                    |
| `file-id`         | string | Yes      | File identifier from the file list endpoint       |
| `bypass-encoding` | string | No       | Set to `"plain"` to bypass encoding on retry      |

**curl examples:**

Basic download:

```bash
curl -X POST https://api.internetofwildlife.com/station/api/download-file/ \
  -H "Content-Type: application/json" \
  -d '{"token": "YOUR_API_TOKEN", "file-id": "YOUR_FILE_ID"}'
```

With bypass encoding (fallback on error):

```bash
curl -X POST https://api.internetofwildlife.com/station/api/download-file/ \
  -H "Content-Type: application/json" \
  -d '{"token": "YOUR_API_TOKEN", "file-id": "YOUR_FILE_ID", "bypass-encoding": "plain"}'
```

**Response:** Raw file content (CSV text, UTF-8 encoded)

**Used by:**
- `download_files()` in `R/download_files.R`
- Called from `get_data()` in `R/get_data.R` for each file in the download queue

**Errors:**

| Error | Cause | What You See |
|-------|-------|--------------|
| HTTP 401 Unauthorized | Invalid or expired API token | `httr::stop_for_status()` raises an error: `"Unauthorized (HTTP 401)"` |
| HTTP 503 Service Unavailable | Server overloaded or temporarily down | Warning: `"Server query timeout — try narrowing your date range or using pagination"`, then execution stops |
| Connection timeout | Network issue or server unreachable | R error: `"Timeout was reached"` after 60 seconds |
| Encoding error (first attempt) | File content has encoding issues the server can't handle | `download_files()` prints `"Here's the original error message:"` followed by the error detail, then automatically retries with `bypass-encoding = "plain"` |
| Encoding error (retry also fails) | File is corrupted or unrecoverable | The retry request's error propagates up; `get_data()` catches it per-file and records `status = "failed"` with the error message in the results dataframe |
| Invalid file ID | `file-id` does not match any file on the server | The API returns an error status; `httr::stop_for_status()` stops execution |
| File download fails in batch | Any error during a single file download within `get_data()` | The file is marked as `"failed"` in the results dataframe; remaining files continue downloading. Final summary prints: `"Download complete: X succeeded, Y failed out of Z files"` |

---

## Generic POST Wrapper

All endpoints (except direct calls in `get_my_projects` and `project_list`) go through the `post()` function in `R/post.R`:

```r
post(endpoint, payload = NULL, show_progress = FALSE)
```

- Automatically injects `token = my_token` into every request
- 60-second timeout on all requests
- Detects HTTP 503 and emits a warning suggesting narrower date ranges or pagination
- Calls `httr::stop_for_status()` to raise on other HTTP errors
- Optional progress bar via `httr::progress()`

---

## Typical Workflow

```
1. Authenticate          load_dot_env() -> Sys.getenv("MY_TOKEN")
2. List projects         POST /station/api/projects
3. List stations         POST /station/api/stations/        (per project)
4. List files            POST /station/api/file-list         (per station, paginated)
5. Download files        POST /station/api/download-file/    (per file)
6. (Optional) Import     file_handle() -> db_prep() -> db_insert()
```

The `get_my_data()` function in `R/get_my_data.R` orchestrates this entire workflow in a single call.
