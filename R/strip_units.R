#' strip attributes from a vector if they exist
#' @param .x vector with units
#' @export
strip_units <- function(.x) {
  classes <- class(.x)
  attribs <- names(attributes(.x))
  if ("pmunits" %in% attribs) {
    attr(.x, "pmunits" ) <- NULL
    rm_class <- which(classes == "pmunits")
    if (length(rm_class)) {
      class(.x) <- classes[-rm_class]
    }
  }
  return(.x)
}

#' strip all unit columns from a dataframe
#' @param .df dataframe to strip units attributes
#' @export
strip_units_df <- function(.df) {
  .df[] <- lapply(.df, function(.c) {
    strip_units(.c)
  })
  return(.df)
}
