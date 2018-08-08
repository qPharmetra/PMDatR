#' convert posixct to date
#' @param dtc date time column
#' @export
posix_to_date <- function(dtc) {
  as.Date(dtc)
}
