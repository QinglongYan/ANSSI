% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stationary.R
\name{get_stationary}
\alias{get_stationary}
\title{Get near-stationary states}
\usage{
get_stationary(datasets, not = 0.2, hot = 0.4, xot = 0.6,
  ndr = NULL, hdr = NULL, xdr = NULL, gapt, relation)
}
\arguments{
\item{datasets}{The candidate intervals in list format}

\item{not}{Normal occupancy threshold (0.2 by default)}

\item{hot}{High occupancy threshold (0.4 by default)}

\item{xot}{Extremely high occupancy threshold (0.6 by default)}

\item{ndr}{The minimum acceptable duration in normal-occupancy periods (10 by default)}

\item{hdr}{The minimum acceptable duration in high-occupancy periods (4 by default)}

\item{xdr}{The minimum acceptable duration in extremely high-occupancy periods (2.5 by default)}

\item{gapt}{Gap threshold}

\item{relation}{Duration-occupancy relations; optional choices are "stepwise", "linear", "polynomial", and "unitgap"}
}
\value{
List of near-stationary states
}
\description{
This function is applied to identify near-stationary states from candidate intervals based on the modified Cassidy's method
}
