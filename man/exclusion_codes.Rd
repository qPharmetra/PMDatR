% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transforms_fill.R
\name{exclusion_codes}
\alias{exclusion_codes}
\title{Create an exclusion column}
\usage{
exclusion_codes(.data, .col, ..., method = "first", default = "OK")
}
\arguments{
\item{.data}{A data_frame type object}

\item{.col}{The unquoted name of the column to create}

\item{...}{Name-value pairs of conditions to use in creating codes}

\item{method}{one of "first" or "last", determining which code to use in
case multiple conditions are true}

\item{default}{The value to use if no conditions are true.  Defaults to "OK".}
}
\value{
the modified .data data_frame
}
\description{
Create an exclusion column
}
\examples{
library(dplyr)
data_frame(ID=1:3,SEX=c("M","F","X")) \%>\%exclusion_codes(EXCL, BADSEX=SEX=="X", remove.subject=ID==3)
data_frame(ID=1:3,SEX=c("M","F","X")) \%>\%exclusion_codes(EXCL, BADSEX=SEX=="X", remove.subject=ID==2)
}
