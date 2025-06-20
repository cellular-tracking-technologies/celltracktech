% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_track_error.R
\name{map_track_error}
\alias{map_track_error}
\title{Map Track Error}
\usage{
map_track_error(
  node_locs,
  track_error_df,
  sidekick_df,
  tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
)
}
\arguments{
\item{node_locs}{Node locations dataframe}

\item{track_error_df}{Track error dataframe obtained by calc_track_error function}

\item{sidekick_df}{Sidekick calibration test}

\item{tile_url}{Map URL}
}
\value{
map
}
\description{
Map Track Error
}
\examples{
compare_map <- map_track_error(node_locs,
                               track_error_df,
                               sidekick_df,
                               my_tile_url)

}
