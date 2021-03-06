% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_raw.R
\name{extract_raw}
\alias{extract_raw}
\title{Extract 30-sec raw data}
\usage{
extract_raw(inDir, metaDir, outDir, from = NULL, to = NULL, vds)
}
\arguments{
\item{inDir}{The directory where the PeMS raw gz files are located}

\item{metaDir}{The directory where meta data files are located. Meta data files should cover the selected period.}

\item{outDir}{The directory to save the extracted csv files}

\item{from}{From date in the format of YYYY_MM_DD}

\item{to}{To date in the format of YYYY_MM_DD}

\item{vds}{A vector of vehicle detector station (VDS) ID's}
}
\value{
Extracted raw datasets in csv format
}
\description{
Extract 30-sec raw data of selected vehicle detector stations (VDS's) from gz files downloaded from PeMS data clearinghouse and save the daily extracted datasets in the format of csv.
}
