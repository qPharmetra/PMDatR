#' @export
convert <- function(x, ...) {
  UseMethod("convert")
}

convert.default <- function(x, .to) {
  set_units(x, .to)
}

#' convert units to a chosen value
#' @param x unit column
#' @param .to units to convert to (can be string, or a vector with the first element the units and the second
#' element named "MW" containing the moleclar weight in g/mol)
#' @rdname convert
#' @export
convert.pmunits <- function(x, .to, .from=attr(x, "pmunits")) {
  # look for MW in .to (could be a vector with named MW, or could just be the unit as string)
  MW=as.numeric(.to["MW"])
  .to=.to[1]

  if(!is.na(MW)){
    MW=set_units(MW,"g/mol")
    #figure out whether to multiply or divide MW
    if(udunits2::ud.are.convertible(sprintf("%s*g/mol",.from),.to)) x=x*MW
    if(udunits2::ud.are.convertible(sprintf("%s/(g/mol)",.from),.to)) x=x/MW
    # note if neither of these work, just let it pass through
  }
  x <- udunits2::ud.convert(x, attr(x,"pmunits"), .to)
  attr(x, "pmunits") <- .to
  return(x)
}

#' convert_units_from_list converts units for all output columns provided
#' @param .df dataframe
#' @param .ul unit list with name/value
#' @importFrom dplyr intersect
#' @examples
#' library(dplyr)
#' Theoph <- set_units_from_list(Theoph, list(Wt = "kg", conc = "mg/L", Dose = "mg/kg"))
#' Theoph <- Theoph %>% mutate(Amt = Dose*Wt)
#' convert_units_from_list(Theoph, list(Amt = "g"))
#' @export
convert_units_from_list <- function(.df, .ul) {
  # if(strict) {
  #   .missing <- setdiff(names(ul), names(.df))
  #   if(length(missing)) {
  #     stop(paste("missing columns for unit conversion:", paste0(.missing, collapse = ",")))
  #   }
  # }
  unit_col_names <- intersect(names(.ul), names(.df))
  for (i in seq_along(unit_col_names)) {
    .unit_name <- unit_col_names[[i]]
    .df[[.unit_name]] <- convert(.df[[.unit_name]], .ul[[.unit_name]])
  }
  return(.df)
}
