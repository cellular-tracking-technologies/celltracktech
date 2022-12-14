% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newdb.R
\name{db_cleanup}
\alias{db_cleanup}
\title{Remove duplicates}
\usage{
db_cleanup(conn)
}
\arguments{
\item{conn}{the connection to your local database}
}
\description{
This function allows you to remove records for more than one beep on the same node at the same time. Additionally, it gets rid of all records where RSSI varies for the same beep record
(combination of time, tag ID and node).
}
