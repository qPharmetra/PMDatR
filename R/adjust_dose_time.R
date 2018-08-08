#' adjust the dose time
#' @details
#' given a row derived from a missing dose, the time would have already been
#' properly adjusted, hence should not tweak, however, to maintain consistency
#' new ADDL rows time-of-day should match the next dose time-of-day, so 
#' must adjust to reflect
#' eg given a next dose time of 7:33
#' eg record came from missing would already be set to 7:33 - don't adjust
#' dose record from prior valid dose set to 10:12 --> adjust to actually dose at 7:33
#' @param .x time
#' @param .adjusted whether adjustment had already occured
#' @param .next_dose_time vector of value(s) to adjust to
adjust_dose_time <- function(.x, .adjusted, .next_dose_time) {
  ifelse(.adjusted, 0, .next_dose_time - seconds_into_day(.x))
}