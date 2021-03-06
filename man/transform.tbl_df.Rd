% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transforms_fill.R
\name{transform.tbl_df}
\alias{transform.tbl_df}
\title{Add new variables to a data_frame}
\usage{
\method{transform}{tbl_df}(.data, ...)
}
\arguments{
\item{.data}{The data_frame object to apply the transformation to}

\item{...}{Name-value pairs of expressions. Use NULL to drop a variable.}
}
\value{
A copy of .data with the transformed columns
}
\description{
Add new variables to a data_frame
}
\details{
This function provides a wrapper around \code{dplyr::mutate}.  It implements the \code{base::transform}
generic for data_frame objects.
}
\examples{
library(dplyr)
CO2 \%>\% as_data_frame \%>\% transform(newType=Type)
}
