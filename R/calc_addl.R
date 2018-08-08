
should_zero_addl <- function(.dtime, .ii) {
  return(is.na(.dtime) | .ii == 0 | is.na(.ii))
}

#' calc_addl is a vectorized function which provides the number of additional
#' doses given an time range and interdose interval
#' @param .dtime time difference
#' @param .ii interdose interval
#' @param .f callback on now to handle non-nominal intervals, defaults to round
#' @details
#'
#' To handle non-nomincal times is through the function used to figure out
#' the number of addl's needed. The default is to floor the time/ii, so
#' given an interval of 30 hours and an II of 12, 2.5 doses would be needed to cover
#' the interval --> flooring would provide 2 doses, therefore addl of 1 would be returned.
#' However, this behavior could be changed to use ceiling to be inclusive,
#' as a ceiling(2.5) --> 3 doses, therefore addl of 2 would be returned.
#'
#' Even more customized functions could be used, however be sure they are vectorized.
#' @export
calc_addl <- function(.dtime, .ii, .f = round, .tol=0.5) {
  ## should likely add checks to time types
  addls <- ifelse(should_zero_addl(.dtime, .ii),
           0,
           # need to subtract 1 time for the initial dose
           # with tolerance .5 should shift by zero, with +.5 and -.5 for 1 and 0
           .f(as_numeric(.dtime)/.ii - 1 - (.tol-.5))
         )
  return(addls)
}
