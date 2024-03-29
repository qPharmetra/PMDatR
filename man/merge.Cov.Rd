% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_functions.R
\name{merge.Cov}
\alias{merge.Cov}
\title{Merge events with covariates}
\usage{
\method{merge}{Cov}(events.df, covs.l)
}
\arguments{
\item{events.df}{The events dataframe into which to merge the covariates}

\item{covs.l}{A list of covariate tbl_df dataframes to merge into events.df}
}
\value{
A tbl_df object with the merged data
}
\description{
Merge events with covariates
}
\details{
The events dataframe is merged with each tbl_df according to the
keys for the covariates.  Each member of covs.l may have its own keys, which
are stored in the groups property for the object.  Hence, merging can be controlled
by changing the grouping of the covariate dataframes using dplyr::group_by.

By default a left join is performed, so all records in events.df are preserved.  Unmatched key
values in the covariate data will result in NA in the covariate column.  Covariate data
with unmatched keys will be silently dropped.  For example, if merging covariates by
ID and VISIT, if the SCREENING visit was not included in the covariate data then
observations or doses occurring in (or associated with) the SCREENING VISIT will have
NA values for the covariates.  Conversely, if the Follow-up VISIT is not present in the event
data, no covariate data is present in the result for the Follow-up visit.  This ensures that
new rows are not added that would not have the correct variables (TIME, AMT, DV, EVID, etc.)
set.

To use other types of joins, set a join_type attribute on the data_frame objects in covs.l.
The available options are: left, right, inner, full.  Any other value will default to a left join.
}
