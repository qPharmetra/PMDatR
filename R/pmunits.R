#' retrieve measurement units from \code{pmunits} object
#' @export
units.pmunits <- function(x) {
  attr(x, "pmunits")
}

#' @export
units.default <- function(x) {
  NULL
}
