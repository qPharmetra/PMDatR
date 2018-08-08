#' lookup intervals is a vectorized function that returns
#' the intervals associated with provided dosing frequency information
#' @param .freq vector of frequencies to provide II's
#' @param .freq_list named list of frequencies and their corresponding II
#' @details 
#' .freq_list allows one to override the default values. The current supported intervals are:
#' list(
#'   "ONCE" = 0,
#'   "QD" = 24,
#'   "BID" = 12,
#'   "TID" = 8
#' )
#' 
#' If a frequency value is provided, but not in the frequency list, an NA value is returned
#' @examples 
#' lookup_intervals("ONCE")
#' lookup_intervals(c("ONCE", "BID", "TID"))
#' lookup_intervals(c("ONCE", "NOTRECOGNIZED", "BID", "TID"))
#' @export
lookup_intervals <- function(.freq, .freq_list = NULL) {
  if (is.null(.freq_list)) {
    freqs <- list(
    "ONCE" = 0,
    "QD" = 24,
    "BID" = 12,
    "TID" = 8
    )
  }
  purrr::map_dbl(.freq, function(fr) {
    ifelse(fr %in% names(freqs), freqs[[fr]], NA_real_)  
  })
}