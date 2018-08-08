#' set_units sets the units for a given vector
#' @param x vector of values
#' @param units units
#' @examples
#' dose <- set_units(c(100, 100), "mg/kg")
#' @export
set_units <- function(x, units, strict=F) {
  #make sure if this is called from convert that MW doesn't mess things up
  .units=units[1]
  if(is.null(.units)) return(strip_units(x)) #NULL units means to unset
  if (!parseable(.units)) {
    if(strict) stop("units not parseable")
    #warning(sprintf("units '%s' are not paresable.  Setting anyway, but try resetting after manual conversion",.units))
    warning(sprintf("units '%s' are not paresable.  Units not set, but try setting after manual conversion.",.units))
    return(x)
    }
#   if(!is.numeric(x)){
#     if(strict) stop("non-numeric vectors cannot have units.")
#     warning("non-numeric vectors cannot have units.")
#     return(x)
#   }
  attr(x, "pmunits") <- .units
  if (!("pmunits" %in% class(x))) {
    class(x) <- c("pmunits", class(x))
  }
  return(x)
}


#' set_units_from_list sets units for all columns that match a name
#' @param .df dataframe to set units
#' @param .ul unit list with name/value
#' @importFrom dplyr intersect
#' @examples
#' set_units_from_list(Theoph, list(Wt = "kg", conc = "mg/L", Dose = "mg/kg"))
#' @export
set_units_from_list <- function(.df, .ul) {
 # if(strict) {
 #   .missing <- setdiff(names(ul), names(.df))
 #   if(length(missing)) {
 #     stop(paste("missing columns for unit conversion:", paste0(.missing, collapse = ",")))
 #   }
 # }
  unit_col_names <- intersect(names(.ul), names(.df))
  for (i in seq_along(unit_col_names)) {
    .unit_name <- unit_col_names[[i]]
    .df[[.unit_name]] <- set_units(.df[[.unit_name]], .ul[[.unit_name]])
  }
  return(.df)
}
