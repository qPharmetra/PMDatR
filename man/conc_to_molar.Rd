% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/molar_conversions.R
\name{conc_to_molar}
\alias{conc_to_molar}
\title{conc_to_molar converts a concentration to molar}
\usage{
conc_to_molar(conc, mol_weight, final_units = NULL)
}
\arguments{
\item{conc}{concentration value with units set}

\item{mol_weight}{molecular weight, defaults to g/mol if no units assigned}

\item{final_units}{output units, defaults to mol/L}
}
\description{
conc_to_molar converts a concentration to molar
}
\examples{
conc_to_molar(set_units(10, "mg/L"), set_units(50, "g/mol"), "umol/L")
conc_to_molar(set_units(10, "mg/L"), set_units(50, "g/mol"))
}
