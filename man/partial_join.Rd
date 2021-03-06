% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataframe_utilities.R
\name{partial_join}
\alias{partial_join}
\title{Join two dataframes with partial matching of keys}
\usage{
partial_join(x, y, by_x, pattern_y)
}
\arguments{
\item{x}{A dataframe}

\item{y}{A dataframe}

\item{by_x}{Column to join in x}

\item{pattern_y}{Column to join in y, containing patterns which may result in multiple matches}
}
\value{
A dataframe
}
\description{
Join two dataframes with partial matching of keys
}
\details{
If by_x has multiple values that match rows of pattern_y, multiple rows are created
by the join.
}
\examples{
library(dplyr)
x=data_frame(A=c("AA BB","CC","DD EE"), B=1:3)
y=data_frame(K=c("AA","BB","CC","DD"), Z=c("cat1","cat1","cat1","cat2"))
partial_join(x,y,"A","K")
}
