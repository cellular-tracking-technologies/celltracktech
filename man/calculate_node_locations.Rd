% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_node_locations.R
\name{calculate_node_locations}
\alias{calculate_node_locations}
\title{Calculate Node Locations}
\usage{
calculate_node_locations(node_health_df)
}
\arguments{
\item{node_health_df}{Node Health dataframe, obtained from 'node_health' data table}
}
\value{
result - dataframe with average and standard deviations of latitude and longitude
}
\description{
Calculate Node Locations
}
\examples{
node_locs <- calculate_node_locations(node_health_df)
}
