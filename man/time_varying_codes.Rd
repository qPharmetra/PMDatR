% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataframe_utilities.R
\name{time_varying_codes}
\alias{time_varying_codes}
\title{Convert a dataframe of codes with start/stop time to a flat, time-varying covariate format}
\usage{
time_varying_codes(df, codes, stdtc, endtc, start_val = 1, end_val = 0,
  TIME = "TIME", .sticky = NULL)
}
\arguments{
\item{df}{A dataframe (e.g. CM or AE domain)}

\item{codes}{The name of a column that holds codes that will become columns}

\item{stdtc}{An expression for the start time (e.g. CMSTDTC)}

\item{endtc}{An expression for the end time (e.g. iso_to_posix(CMSTDTC, .time="24:00"))}

\item{start_val}{An expression for the value to assign at the start time}

\item{end_val}{An expression for the value to assign at the end time}

\item{TIME}{A name for the new time column}

\item{.sticky}{A value (set by start_val or end_val) which should be set permanently}
}
\value{
A reformatted dataframe
}
\description{
Convert a dataframe of codes with start/stop time to a flat, time-varying covariate format
}
\details{
The function does the heavy lifting of separating codes into columns that are
flagged at their start and stop times.  The names of the columns are the codes provided in
df.  There is some setup that may be necessary to make proper use of this function:
\itemize{
   \item Set grouping before calling.  The groups will be preserved.
   \item The times are assumed to be POSIXct or numeric.  Conversions can be done using the
   mappings stdtc and endtc.
   \item Clean up the data to remove duplicate rows and create short codes that will make
   suitable column names.
   \item filter out codes that are not of interest.  The function will create columns for
   ALL of the codes.
   \item the output of this function is inteded to be used in a Time-Varying Covariate query.
   Records occurring beyond the limits of the study can be removed there.  The output keeps
   the original data on the rows to aid in further pruning/mapping of the dataset.
   \item The start_val and end_val mappings can be complex expressions to take into account
   not only the codes, but other columns as well.  The values can be set arbitrarily.
}
}
\examples{
\dontrun{
cm \%>\% filter(CMDECOD \%in\% .(ATORVASTATIN,PARACETAMOL,IBUPROFEN,METFORMIN)) \%>\%
  group_by(USUBJID) \%>\%
  time_varying_codes(CMDECOD,
    iso_to_posix(CMSTDTC,.time = "00:00"),
    iso_to_posix(CMENDTC,.time = "24:00"),
    ifelse(CMENDTC=="",2,1),
    .sticky = 2) -> cm.out
}
}
