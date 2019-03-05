#' Test where a date falls relative to a date range
#'
#' @param target The date of interest
#' @param start The starting date of the range
#' @param end The ending date of the range
#' @param .before The value to assign if the date falls before the range (default FALSE)
#' @param .ongoing The value to assign if the date falls after start, and end is NA (default TRUE)
#' @param .during The value to assign if the date falls within the range (default TRUE)
#' @param .after The value to assign if the date falls after the range (default FALSE)
#'
#' @details The return values can be logical, character, or numeric.  The interval for within includes
#' the start and end dates.
#' @return The return type is determined by the user.  The default is logical.
#'
#' @export
#'
#' @examples
#' date_in_range(iso_to_posix("2018-03-14T12:34"), iso_to_posix("2018-03-11T12:34"), iso_to_posix("2018-03-18T12:34"))
#' date_in_range("2018-03-14", "2018-03", "2018-03-18T12:34")
#' date_in_range(iso_to_posix("2018-03-14T12:34"), iso_to_posix("2018-03-15T12:34"), iso_to_posix("2018-03-18T12:34"))
#' date_in_range(iso_to_posix("2018-03-14T12:34")+(1:10)*24*2600,
#'   iso_to_posix("2018-03-11T12:34"),
#'   iso_to_posix("2018-03-18T12:34"))
#' date_in_range(iso_to_posix("2018-03-09T12:34")+(1:10)*24*3600,
#'   iso_to_posix("2018-03-11T12:34"),
#'   c(rep(iso_to_posix("2018-03-15T12:34"),9), NA),
#'   .before="BEFORE", .ongoing="ONGOING", .during="DURING", .after="AFTER")
date_in_range = function(target, start, end,  .before=FALSE, .ongoing=TRUE, .during=TRUE, .after=FALSE){
  # for now assume all dates in POSIXct
  target = iso_to_posix(target)
  start = iso_to_posix(start)
  end = iso_to_posix(end)
  values = c(.before, .ongoing, .during, .after)
  val = conditional_values("1"=target<start,
                           "2"=target>=start & is.na(end),
                           "3"=target>=start & target<=end,
                           "4"=target>end)
  values[as.numeric(val)]
}

#' Check if a value matches multiple regular expressions
#'
#' @param x A character vector
#' @param regexps A vector of regular expressions
#' @param ignore.case A logical.  If FALSE (default) case is ignored in the comparison
#'
#' @return A logical vector of length x
#' @export
#'
#' @details The vector x is checked against each regular expression in the array provided.  Any match
#' results in TRUE for the element
#' @examples
#' nams=c("George Washington", "Washington DC", "King George")
#' contains_one_of_(nams, c("Washington","DC"))
contains_one_of_ = function(x, regexps, ignore.case=FALSE){
  rexp = paste0(regexps, collapse = "|")
  grepl(rexp, x, ignore.case=ignore.case)
}

#' Check if a value matches multiple regular expressions
#'
#' @param x A character vector
#' @param ... unquoted or quoted regular expressions
#' @param ignore.case A logical.  If FALSE (default) case is ignored in the comparison
#'
#' @return The vector x is checked against each regular expression in the array provided.  Any match
#' results in TRUE for the element
#' @export
#'
#' @examples
#' nams=c("George Washington", "Washington DC", "King George")
#' contains_one_of(nams, Washington, DC)
#' contains_one_of(nams, ing)
#' contains_one_of(nams, "^Wash.*$")
contains_one_of = function(x, ..., ignore.case=FALSE ){
  .dots = sapply(purrr::map(lazyeval::lazy_dots(..., .follow_symbols=TRUE), "expr"), as.character)
  contains_one_of_(x, .dots, ignore.case)
}
