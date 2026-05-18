# Data API Performance Update — R Package Impact Assessment

**Date**: 2026-03-24
**Prepared for**: Jessica Gorzo (celltracktech R package maintainer)
**Prepared by**: Bob Fogg

---

## Background

The `data-app-api-server` `/station/api/file-list` endpoint has been experiencing query times of 30–280 seconds for researchers with large multi-year projects. This causes `httr` client timeouts, nginx 499 errors, and failed data downloads. We've deployed server-side improvements and the API now supports optional pagination parameters.

---

## Server Changes Deployed

### 1. Query Timeout (120 seconds)
Queries exceeding 120 seconds now return HTTP `503` with a JSON error body instead of hanging for 4+ minutes. Previously, slow queries would hold the connection until the R client's `httr::timeout(3000)` expired or the researcher killed the session.

### 2. Optional Pagination (`limit` / `offset`)
Both `/station/api/file-list` and `/station/api/all-user-files/` now accept optional `limit` (max: 2000) and `offset` parameters in the request body. **When omitted, behavior is unchanged** — the full result set is returned.

### 3. Token Caching
Django auth token validation is now cached server-side for 5 minutes. Batch workflows (e.g., `get_my_data()` calling `getStationFileList()` once per station, then `downloadFiles()` per file) no longer hit the database for token validation on every request.

---

## Current R Package Analysis

### Affected Functions in `R/api_postgres.R`

**`getStationFileList()` (line 133)**
```r
getStationFileList <- function(station_id, begin, filetypes = NULL, end = NULL) {
  endpoint <- files  # "/station/api/file-list"
  payload <- list("station-id" = station_id, begin = as.Date(begin))
  # ...
  return(httr::content(post(endpoint = endpoint, payload = payload)))
}
```
- **No pagination**: Does not send `limit` or `offset`
- **Impact**: Returns all files for a station deployment. For multi-year deployments with thousands of files, this is the query causing 30–280s response times.
- **Current behavior post-update**: Still works (unbounded), but may hit the new 120s timeout on very large stations. Previously it would hang silently.

**`post()` helper (line 118)**
```r
post <- function(endpoint, payload = NULL) {
  payload_to_send <- list(token = my_token)
  if (!is.null(payload)) {
    payload_to_send <- c(payload_to_send, payload)
  }
  response <- httr::POST(host, path = endpoint, body = payload_to_send,
                          encode = "json", httr::timeout(3000))
  httr::stop_for_status(response)
  return(response)
}
```
- **Timeout**: 3000 seconds (50 minutes!) — extremely generous. With the server now capping at 120s, this will never fire for slow queries. The client will get a 503 instead.
- **`stop_for_status()`**: Will throw an error on the new 503 response. This is the main behavioral change researchers might notice.

**`get_data()` (line 836)** — the main download orchestrator
```r
files_avail <- lapply(my_stations[["stations"]], function(station, ...) {
  # ...
  file_info <- do.call(getStationFileList, kwargs)  # line 869
  outfiles <- file_info[["files"]]
  return(outfiles)
})
```
- Calls `getStationFileList()` once per station in a `lapply` loop
- Each call is unbounded — returns all files for that station
- For a project with 20 stations × 3 years of data, this is 20 sequential unbounded API calls

**`downloadFiles()` (line 150)**
```r
downloadFiles <- function(file_id) {
  endpoint <- "/station/api/download-file/"
  payload <- list("file-id" = file_id)
  response <- tryCatch({
    post(endpoint = endpoint, payload = payload)
  }, error = function(cond) {
    payload <- c(payload, "bypass-encoding" = "plain")
    post(endpoint = endpoint, payload = payload)
  })
  return(response)
}
```
- Downloads one file at a time — not affected by pagination changes
- The `tryCatch` fallback (bypass-encoding on error) is good resilience
- Token caching improvement helps here since this is called hundreds of times per session

---

## What Will Break

**Nothing breaks immediately.** All existing calls without `limit`/`offset` return the full result set as before.

**However**, researchers with very large station deployments (3+ years, thousands of files) may now see a 503 error where they previously saw a very slow (but eventual) response. The `stop_for_status()` in `post()` will convert this to an R error:

```
Error in httr::stop_for_status(response) :
  503 Service Unavailable
```

This is better than the old behavior (hanging for 4+ minutes then timing out) but the R package should handle it gracefully.

---

## Recommended R Package Changes

### Priority 1: Handle 503 timeout gracefully in `post()`

```r
post <- function(endpoint, payload = NULL) {
  payload_to_send <- list(token = my_token)
  if (!is.null(payload)) {
    payload_to_send <- c(payload_to_send, payload)
  }
  response <- httr::POST(host, path = endpoint, body = payload_to_send,
                          encode = "json", httr::timeout(60))  # reduce from 3000 to 60
  if (httr::status_code(response) == 503) {
    warning("Server query timeout — try narrowing your date range or using pagination")
  }
  httr::stop_for_status(response)
  return(response)
}
```

### Priority 2: Add pagination support to `getStationFileList()`

```r
getStationFileList <- function(station_id, begin, filetypes = NULL,
                                end = NULL, limit = NULL, offset = NULL) {
  endpoint <- files
  payload <- list("station-id" = station_id, begin = as.Date(begin))

  if (!is.null(filetypes)) {
    add_types <- filetypes[filetypes %in% file_types]
    if (length(which(!filetypes %in% file_types)) > 0) {
      print(paste("WARNING: invalid file type specified - ignoring:",
                  filetypes[!filetypes %in% file_types]))
    }
    payload[["file-types"]] <- add_types
  }
  if (!is.null(end)) {
    payload[["end"]] <- as.Date(end)
  }
  if (!is.null(limit)) {
    payload[["limit"]] <- as.integer(limit)
  }
  if (!is.null(offset)) {
    payload[["offset"]] <- as.integer(offset)
  }
  return(httr::content(post(endpoint = endpoint, payload = payload)))
}
```

### Priority 3: Add a paginated wrapper for large stations

```r
#' Get all files for a station with automatic pagination
#'
#' For stations with many files, retrieves results in pages to avoid
#' server timeouts. Falls back to unbounded request for small stations.
#'
#' @param station_id station identifier
#' @param begin start date
#' @param filetypes optional file type filter
#' @param end optional end date
#' @param page_size number of files per page (default 500)
#' @return list of file metadata
#' @export
getStationFileListPaginated <- function(station_id, begin, filetypes = NULL,
                                         end = NULL, page_size = 500) {
  all_files <- list()
  offset <- 0
  repeat {
    result <- getStationFileList(station_id, begin,
                                  filetypes = filetypes, end = end,
                                  limit = page_size, offset = offset)
    batch <- result[["files"]]
    if (is.null(batch) || length(batch) == 0) break
    all_files <- c(all_files, batch)
    if (length(batch) < page_size) break
    offset <- offset + page_size
  }
  return(list(files = all_files))
}
```

### Priority 4: Update `get_data()` to use pagination

In `get_data()` line 869, replace:
```r
file_info <- do.call(getStationFileList, kwargs)
```
with:
```r
file_info <- do.call(getStationFileListPaginated, kwargs)
```

This is backward compatible — small stations return in one page, large stations paginate automatically.

---

## Migration Strategy

| Phase | Change | Breaking? | Timeline |
|-------|--------|-----------|----------|
| 1 (deployed) | Server: 120s timeout + optional pagination | No | Done |
| 2 | R package: handle 503 in `post()`, reduce timeout | No | Next release |
| 3 | R package: add `limit`/`offset` to `getStationFileList()` | No | Next release |
| 4 | R package: add `getStationFileListPaginated()` | No (additive) | Next release |
| 5 | R package: switch `get_data()` to use paginated variant | No (same results) | Next release |

All changes are backward compatible. Researchers using older R package versions continue to work — they just won't benefit from pagination until they update.

---

## Testing Checklist

- [ ] `get_my_data()` with a small project (< 100 files) — should work identically
- [ ] `get_my_data()` with a large project (1000+ files, 3+ years) — should now succeed where it previously timed out
- [ ] `update_db()` incremental update — should work identically (only downloads new files)
- [ ] `getStationFileList()` without limit/offset — returns full result (backward compat)
- [ ] `getStationFileList()` with limit=50, offset=0 — returns first 50 files
- [ ] Server returns 503 on very large unbounded query — R package shows warning, not silent hang
- [ ] `downloadFiles()` — unchanged behavior, but faster due to token caching
