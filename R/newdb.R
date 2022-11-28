#' Remove duplicates
#'
#' This function allows you to remove records for more than one beep on the same node at the same time. Additionally, it gets rid of all records where RSSI varies for the same beep record
#' (combination of time, tag ID and node).
#' @param conn the connection to your local database
#' @export

db_cleanup <- function(conn) {
  DBI::dbExecute(conn, "WITH ordered AS (
  SELECT id, time, tag_id, node_id, tag_rssi,
    rank() OVER (PARTITION BY time, tag_id, node_id, tag_rssi  ORDER BY id) AS rnk
  FROM raw where node_id is not null
),
to_delete AS (
  SELECT *
  FROM   ordered
  WHERE  rnk > 1
)
delete from raw using to_delete where raw.id = to_delete.id")

DBI::dbExecute(conn, "WITH ordered AS (
  SELECT id, time, tag_id, node_id, tag_rssi,
    rank() OVER (PARTITION BY time, tag_id, node_id  ORDER BY id) AS rnk
  FROM raw where node_id is not null
),
to_delete AS (
  SELECT *
  FROM   ordered
  WHERE  rnk > 1
)

delete from raw using to_delete where raw.time = to_delete.time and raw.node_id = to_delete.node_id and raw.tag_id =to_delete.tag_id") #2022-04-04 19:43:43-04 1933552D 377c59
}
