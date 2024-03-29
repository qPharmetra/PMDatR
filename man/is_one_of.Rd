% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transforms_time.R
\name{is_one_of}
\alias{is_one_of}
\title{Test if value in x is one of several values}
\usage{
is_one_of(x, ...)
}
\arguments{
\item{x}{A vector to test}

\item{...}{quoted strings to test against}
}
\value{
A vector of logical (TRUE or FALSE) of length(x)
}
\description{
Test if value in x is one of several values
}
\details{
This is a functionalized form of x %in% c(...)
}
\examples{
is_one_of(letters[1:5],"c","g")
}
