% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep.data.R
\name{prep.data}
\alias{prep.data}
\title{Prepare Data}
\usage{
prep.data(x, y, SLIDE.TIME, GROUP.TIME, K, a, S, startval = NULL)
}
\arguments{
\item{x}{Dataframe, raw or blu from data table, filtered for specific tag and specific date}

\item{y}{Nodes dataframe}

\item{SLIDE.TIME}{Slide time}

\item{GROUP.TIME}{Group time}

\item{K}{Horizontal asymptote}

\item{a}{Intercept}

\item{S}{Decay factor}

\item{startval}{NULL}
}
\value{
dataframe
}
\description{
Prepare Data
}
