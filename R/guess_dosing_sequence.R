#' guess dosing sequence based on time of day
#' @param .time_hrs time of day, in hours
#' @param .freq dosing frequency (QD, BID, etc)
#' @param seq_break_times list of time cutpoints, in hours, to use as breakpoints for given sequences
#' @export
#' @details
#' seq_break_times determines how to calculate the sequence
#' defaults are as follows:
#'  seq_break_times <- list(
#'    "QD" = 25, # so always be 1
#'    "BID" = 12, # before noon = seq 1, after noon = seq 2
#'    "TID" = c(10, 16)
#'  )
#'
guess_dosing_sequence <- function(.time_hrs, .ii, seq_break_times = NULL) {
  seq_break_defaults <- list(
    "QD" = 25, # so always be 1
    "BID" = 12, # before noon = seq 1, after noon = seq 2
    "TID" = c(10, 16)
  )
  # frequency is a lookup table, and can be used instead of II to guess what the sequence is,
  # the approach could be valueable if non-nominal II intervals are specified, however for now
  # will just match it up to II
  .freq <- ifelse(.ii == 12, "BID", ifelse(.ii == 24, "QD", ifelse(.ii == 8, "TID", NA)))
  # override any defaults
  if (!is.null(seq_break_times)) {
    for (n in names(seq_break_times)) {
      seq_break_defaults[[n]] <- seq_break_times[[n]]
    }
  }
  dosing_seq <- vector("numeric", length(.time_hrs))
  # yes this is very slow, but should be "easy" to refactor to cpp if we
  # want to go this route for dosing sequence estimates
  for (i in seq_along(.time_hrs)) {
    breaks <- seq_break_defaults[[.freq[i]]]
    if (is.null(breaks) || is.na(.time_hrs[[i]])) {
      # didn't exist, will default to seq 1, though could also do NA or error
      dosing_seq[i] <- 1
      next
    }
    # default to one past any possible break
    # so if not less than any breaks will be the next sequence number
    seq_value <- length(breaks) + 1
    tvalue <- .time_hrs[i]
    for (b in seq_along(breaks)) {
      if (tvalue <= breaks[b]) {
        seq_value <- b
        break # don't check any more or will continually overwrite
      }
    }
    dosing_seq[i] <- seq_value
  }
  return(dosing_seq)
}
