# wrap ud.is.parseable for the moment
parseable <- function(...) {
  udunits2::ud.is.parseable(...)
}

convertible <- function(...) {
  udunits2::ud.are.convertible(...)
}
