#' get the units columns present in a dataframe and what the units are
#' @param .df dataframe
#' @export
#' @examples
#' th1 <- set_units_from_list(Theoph, list(conc = "mg/L", Wt = "kg"))
#' unit_cols(th1)
unit_cols <- function(.df) {
  purrr::reduce(names(.df), function(.acc, .x) {
    if ("pmunits" %in% class(.df[[.x]])) {
      orig_names <- names(.acc)
      .acc <- purrr::set_names(c(.acc, attr(.df[[.x]], "pmunits")),
                               c(names(.acc), .x))
    }
    return(.acc)
  }, .init = c())
}
