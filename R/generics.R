#' @export
`[.pmunits` <- function(x, i, j,..., drop = TRUE)
  structure(NextMethod(), "pmunits" = units(x), class = "pmunits")

# #' @export
# c.pmunits <- function(..., recursive=F){
#   dots = list(...)
#   cunits=units(dots[[1]])
#   conv_vals = purrr::map_if(dots, is_pmunits, convert)
#   set_vals = purrr:map_if(dots, !is_pmunits, set_units, cunits)
#
#   vals = sapply(dots, set_units, units=cunits)
#   structure(vals, "pmunits" = cunits, class="pmunits")
# }

# for tibble summaries
#' @export
type_sum.pmunits <- function(x, ...) {
  units(x)
}
