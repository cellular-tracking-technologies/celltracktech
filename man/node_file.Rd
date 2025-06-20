% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node.R, R/node_file.R
\name{node_file}
\alias{node_file}
\title{Make the node file}
\usage{
node_file(health)

node_file(health)
}
\arguments{
\item{health}{Node health dataframe from 'node_health' data table}
}
\value{
wells_sf - dataframe

health - dataframe
}
\description{
This function allows you to create the node location spatial data frame from your node health data.
}
\examples{
nodes <- node_file(nodehealth)
nodes <- node_file(nodehealth)
}
