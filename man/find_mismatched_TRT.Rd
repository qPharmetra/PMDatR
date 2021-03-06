% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostics.R
\name{find_mismatched_TRT}
\alias{find_mismatched_TRT}
\title{Find mismatched Treatment codes across domains}
\usage{
find_mismatched_TRT(DM, master = "DM")
}
\arguments{
\item{DM}{A data management object}

\item{master}{The name of the master domain as character}
}
\value{
A tbl containing the unmatched treatment columns and codes by
}
\description{
Compares the treatment codes in <master> to those in all other domains.  Treatment code combinations not found
in <master> domain are returned in a table.  Searches any columns that begin with TRT.
}
