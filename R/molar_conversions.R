#' conc_to_molar converts a concentration to molar
#' @param conc concentration value with units set
#' @param mol_weight molecular weight, defaults to g/mol if no units assigned
#' @param final_units output units, defaults to mol/L
#' @examples
#' conc_to_molar(set_units(10, "mg/L"), set_units(50, "g/mol"), "umol/L")
#' conc_to_molar(set_units(10, "mg/L"), set_units(50, "g/mol"))
#' @export
conc_to_molar <- function(
  conc,
  mol_weight,
  final_units = NULL
  ) {
  if (is.null(final_units)) {
    final_units <- "mol/L"
  }
  if (is.null(units(mol_weight))) {
    mol_weight <- set_units(mol_weight, "g/mol")
  }
  convert(conc/mol_weight, final_units)
}
