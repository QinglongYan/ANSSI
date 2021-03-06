% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_filtered.R
\name{merge_filter}
\alias{merge_filter}
\title{Merge filtered datasets}
\usage{
merge_filter(inDir1, inDir2, outDir)
}
\arguments{
\item{inDir1}{The directory where the first half datasets are stored}

\item{inDir2}{The directory where the second half datasets are stored}

\item{outDir}{The directory where the merged datasets are stored}
}
\value{
Merged datasets in the format of csv
}
\description{
This function is applied to bind datasets on the same day from two different VDS's in columns
}
