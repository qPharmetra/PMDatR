#' @name statfuns
#' @aliases mean_ mean
#' @aliases median_ median
#' @aliases sum_ sum
#' @aliases min_ min
#' @aliases max_ max
#' @aliases first_ first
#' @aliases last_ last
#' @aliases nth_ nth
#'
#' @title Summary functions overridden by PMDatR
#'
#' @details Some summary functions have been overridden to make them easier to use from custom function.
#'   All of these skip NA values.  If all values in x are NA, then these functions return NA, rather than
#'   other values such as -Inf, Inf, or NaN
#'
#' @param x An R object with a mean method (e.g. numeric/logical vectors)
#' @param n For 'nth' the selector to use.  Note that NAs are skipped.
#' @param ... Additional arguuments passed to or from other methods
#' @param na.rm TRUE (the default) if NA values should be ignored in the calculation
#'
#' @return The value of the summary function applied to vector
#' @examples
#' mean(c(1,NA,3)) #2
#' median(c(1,NA,3, NA, 5)) #3
#' sum(c(1,NA,3)) #4
#' min(c(1,NA,3, NA, 5)) #1
#' max(c(1,NA,3, NA, 5)) #5
#' @rdname statfuns
#' @export
mean_ <- function(x, ..., na.rm = TRUE) {
  y=base::mean(x, na.rm = na.rm, ...)
  if(is.nan(y)) y=NA
  y
}
#' @rdname statfuns
#' @importFrom stats median
#' @export
median_ <- function(x, na.rm = TRUE) {
  stats::median(x, na.rm = na.rm)
}
#' @rdname statfuns
#' @export
sum_ <- function(..., na.rm = TRUE) {
  base::sum(..., na.rm = na.rm)
}

#' @rdname statfuns
#' @export
min_ <- function(..., na.rm = TRUE) {
  y=base::min(..., na.rm = na.rm)
  if(is.infinite(y)) y=NA
  y
}
#' @rdname statfuns
#' @export
max_ <- function(..., na.rm = TRUE) {
  y=base::max(..., na.rm = na.rm)
  if(is.infinite(y)) y=NA
  y
}
#' @rdname statfuns
#' @importFrom dplyr first
#' @importFrom stats na.omit
#' @export
first_ <- function(x, default=NA){
  dplyr::first(na.omit(x), default=default)
}
#' @rdname statfuns
#' @importFrom dplyr last
#' @importFrom stats na.omit
#' @export
last_ <- function(x,default=NA){
  dplyr::last(na.omit(x), default=default)
}
#' @rdname statfuns
#' @importFrom dplyr nth
#' @importFrom stats na.omit
#' @export
nth_ <- function(x, n, default=NA){
  dplyr::nth(na.omit(x), n, default=default)
}
