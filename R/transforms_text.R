
#' Convert whitespace to a missing value
#'
#' @param x A character vector
#' @param missing_value Defaults to NA, but can be used to convert all whitespace values to a single character value
#'
#' @details Given a character vector, this function replaces entries that contain *only* whitespace with the missing_value
#' provided.  Entries such as empty strings "", any number of spaces "  ", "     ", tab ("\\t"), newline ("\\n"), and
#' carriage return ("\\r") are converted to the missing_value.
#'
#' @return the modified character vector
#' @export
#'
#' @examples
#' blank_to_missing(c("A","B","C"," ", "   ", "\t", " he", "hi\t",123,"\n", "two\nlines", ""))
blank_to_missing = function(x, missing_value=NA){
  gsub("^$|^\\s+$", missing_value, x)
}


#' Recombine pieces of a delimited string
#'
#' @details Intended for use in simplifying USUBJID values, this function takes a column of text USUBJIDs, splits them
#'   by their delimiter ("-") and allows for recombining the parts.
#'
#' @param id The USUBJID values
#' @param pick_parts a numeric value or vector specifying which parts to recombine and in which order
#' @param sep The delimiter between the parts.
#' @param max_parts The maximum number of parts to allow for.
#'
#' @return A vector of the recombined elements from id.
#' @export
#'
#' @examples
#' parse_usubjid(c("1-2-3-4-5","3-28-123-12-41"),5)
#' # "5" "41"
#' parse_usubjid(c("1-2-3-4-5","3-28-123-12-41"),c(1,5))
#' # "15"  "341"
parse_usubjid = function(id, pick_parts, sep="-", max_parts=5){
  sapply(strsplit(id,sep), function(x) {paste(x[pick_parts], collapse="")})
}


#' Give numeric identifiers to a vector of values
#'
#' @details Given a vector of some type, assigns a number to each unique value and returns the
#' vector of the values' positions in the unique values.  The unique values are tabulated in
#' the order of appearance.
#' @param vals The values to enumerate
#'
#' @return the enumeration of vals
#' @export
#'
#' @examples
#' enumerate(letters[1:10])
#' enumerate(rep(letters[1:3], times=5))
enumerate = function(vals){
  as.numeric(as.factor(vals))
}


#' Combine several numbers together into a string
#'
#' @param ... expressions containing numerical values to concatenate
#' @param sep An optional separator to insert between the values in ...
#'
#' @return A character vector with the concatenated values in ...
#' @export
#' @importFrom purrr map
#' @importFrom lazyeval lazy_dots
#'
#' @details Any number of values can be concatenated.  Numerize is
#' meant to work with number-like data, but will only warn if the
#' expressions return non-integer values.  The returned values are character
#' representations of (ideally) numbers, but if non-integer values are included
#' they will still appear.
#'
#' @examples
#' numerize(as.character(1:10), 11:20, sep=".")
#' numerize(as.character(1:10), letters[1:10], sep=".")
#' numerize(as.character(1:10), 11:20/3, sep=".")

numerize = function(..., sep=""){
  # ensure all args are numeric
  # first get the expressions from ... (has to come first or promise is invalid)
  #dotsexpr = purrr::map(lazyeval::lazy_dots(...), "expr")
  # evaluate ...
  dots = list(...)
  # which vectors in ... have entries that can't be coerced to integer
  isnumeric = unlist(lapply(lapply(dots,is.integral),all))
  if(!all(isnumeric)){
    warning("Argument(s) have non integer entries.")#,
                    #paste(dotsexpr[!isnumeric],collapse = ", ")))
  }
  paste(..., sep=sep)
}
