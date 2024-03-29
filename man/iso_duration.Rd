% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transforms_time.R
\name{iso_duration}
\alias{iso_duration}
\title{Convert ISO duration to numeric}
\usage{
iso_duration(x, units = "h")
}
\arguments{
\item{x}{A character vector of ISO durations (e.g. PT23.5H)}

\item{units}{Units to output (defaults to hours)}
}
\value{
A numeric vector of the durations in x, in requested units
}
\description{
Convert ISO duration to numeric
}
\examples{
iso_duration(.(PT1H,-PT1H,PT.5H,-PT.5H,P1D,PT24H,P1W,PT336H))
iso_duration(.(PT1H,-PT1H,PT.5H,-PT.5H,P1D,PT24H,P1W,PT336H), "days")
}
