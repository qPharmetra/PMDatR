% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_addl.R
\name{calc_addl}
\alias{calc_addl}
\title{calc_addl is a vectorized function which provides the number of additional
doses given an time range and interdose interval}
\usage{
calc_addl(.dtime, .ii, .f = round, .tol = 0.5)
}
\arguments{
\item{.dtime}{time difference}

\item{.ii}{interdose interval}

\item{.f}{callback on now to handle non-nominal intervals, defaults to round}
}
\description{
calc_addl is a vectorized function which provides the number of additional
doses given an time range and interdose interval
}
\details{
To handle non-nomincal times is through the function used to figure out
the number of addl's needed. The default is to floor the time/ii, so
given an interval of 30 hours and an II of 12, 2.5 doses would be needed to cover
the interval --> flooring would provide 2 doses, therefore addl of 1 would be returned.
However, this behavior could be changed to use ceiling to be inclusive,
as a ceiling(2.5) --> 3 doses, therefore addl of 2 would be returned.

Even more customized functions could be used, however be sure they are vectorized.
}
