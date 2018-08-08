#' resolve unit requirements across data frames
#' @param .list list of vectors of units
#' @details
#' resolve units will determine the final combination of units across
#' all unit combinations provided as a list of vectors of units
#' @importFrom purrr map
#' @examples
#' resolve_units(list(c(conc = "mg/L"), c(conc = "ug/L")))
#' resolve_units(list(c(conc = "mg/L"), c(conc = "ug/L", dose = "mg")))
#' @export
resolve_units <- function(.list) {
  final_units <- list()
  map(.list, function(.units) {
    if (is.null(.units)) {
      return(invisible())
    }
    map(names(.units), function(.unit) {
      if (is.null(final_units[[.unit]])) {
        final_units[[.unit]] <<- .units[[.unit]]
      }
    })
  })
  return(unlist(final_units))
}
