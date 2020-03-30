#' @export
`[.pmunits` <- function(x, i, j,..., drop = TRUE)
  structure(NextMethod(), class = class(x), "pmunits" = units(x))

#' @export
is.pmunits <- function(x){
  inherits(x, "pmunits")
}

#' @export
c.pmunits <- function(..., recursive=F){
  dots = list(...)
  cunits=units(dots[[1]])
  conv_vals = purrr::map_if(dots, is.pmunits, convert, cunits)
  #set_vals = purrr::map_if(dots, purrr::negate(is.pmunits), set_units, cunits)

  #vals = NextMethod(dots)
  structure(unlist(conv_vals), class= class(conv_vals[[1]]), "pmunits" = cunits)
}

#' @export
print.pmunits <- function(x, ...) {
  NextMethod(x)
}

# for tibble summaries
#' @export
type_sum.pmunits <- function(x, ...) {
  units(x)
}
