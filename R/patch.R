#' Fix database entries
#'
#' This function allows you to copy files with errors or corrupt lines into a new destination to look closer
#' @param d database connection
#' @param outpath where your files were downloaded
#' @param myproject the name of your project at CTT
#' @param dirout where the problem files will be copied
#' @export
#' @examples
#' patch(dbConnect(RPostgres::Postgres(), dbname = db_name), "~/mydata", "My Project", "~/errfiles")
patch <- function(d, outpath, myproject, dirout) {
  # myfiles <- list.files(file.path(outpath, myproject), recursive = TRUE, full.names=TRUE)
  errors <- error_files(file.path(outpath, myproject), dirout, d)
  # myfiles <- names(errors)
  # files_loc <- sapply(strsplit(myfiles, "/"), tail, n=1)
  DBI::dbExecute(d, "UPDATE raw SET node_id=upper(node_id)")
  DBI::dbExecute(d, "UPDATE raw SET tag_id=upper(tag_id)")
  try(DBI::dbSendQuery(conn, "WITH ordered AS (SELECT upper(node_id),
    RANK() OVER (PARTITION BY upper(node_id)) AS rnk
  FROM nodes where node_id is not null
),
to_delete AS (
  SELECT *
  FROM   ordered
  WHERE  rnk > 1
)
delete from nodes using to_delete where nodes.node_id = to_delete.node_id"))
# failed2 <- Map(get_files_import, names(errors), unname(errors), MoreArgs=list(conn=d, fix=T))
}
