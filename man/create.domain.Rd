% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/domain.R
\name{create.domain}
\alias{create.domain}
\title{Create a domain object from file and mappings information}
\usage{
create.domain("EX")
}
\arguments{
\item{name}{The name of the domain.}

\item{path}{The path to the domain data.}

\item{filetype}{The type of file, one of: sas7bdat (default), xpt, csv, txt,
dat, xls, or xlsx.}

\item{filename}{The name (without directory or extension) of the main domain file.
This is defaulted to the name argument.}

\item{premerge.filename}{The name of the premerge file.  If the premerge file
is a CDISC SDTM supplementary domain (defaulted with has.supp=T) then this filename
defaults to 'SUPP'+name (with the appropriate path and extension).}

\item{mappings}{A domain InputMappings object, perhaps loaded from a yaml file, with
which to override default settings.}

\item{has.supp}{Indicates if a CDISC SDTM supplementary domain should be configured
and searched for.}

\item{...}{Additional overrides for file settings, which are passed to various
load routines.  For text files these include: sep (comma, semicolon, tab, whitespace);
header (T/F); fill (T/F); quote (none, single_quote, double_quote); comment.char;
skip.  For Excel files: sheet (name or number of the sheet to read); range
(the cell range to read).  To reshape a text or Excel file, provide the id_cols as a
comma separated single string (it will be parsed later) of column names to keep as
the unique identifiers for the rows.  Columns not in the list will be converted
to name/value pairs in new columns.}
}
\value{
A doman object.
}
\description{
Create a domain object from file and mappings information
}
