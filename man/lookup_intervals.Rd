% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup_intervals.R
\name{lookup_intervals}
\alias{lookup_intervals}
\title{lookup intervals is a vectorized function that returns
the intervals associated with provided dosing frequency information}
\usage{
lookup_intervals(.freq, .freq_list = NULL)
}
\arguments{
\item{.freq}{vector of frequencies to provide II's}

\item{.freq_list}{named list of frequencies and their corresponding II}
}
\description{
lookup intervals is a vectorized function that returns
the intervals associated with provided dosing frequency information
}
\details{
.freq_list allows one to override the default values. The current supported intervals are:
list(
  "ONCE" = 0,
  "QD" = 24,
  "BID" = 12,
  "TID" = 8
)

If a frequency value is provided, but not in the frequency list, an NA value is returned
}
\examples{
lookup_intervals("ONCE")
lookup_intervals(c("ONCE", "BID", "TID"))
lookup_intervals(c("ONCE", "NOTRECOGNIZED", "BID", "TID"))
}
