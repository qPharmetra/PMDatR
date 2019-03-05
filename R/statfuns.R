#' @name statfuns
#' @aliases mean_ mean
#' @aliases median_ median
#' @aliases mode_
#' @aliases sum_ sum
#' @aliases min_ min
#' @aliases max_ max
#' @aliases first_ first
#' @aliases last_ last
#' @aliases nth_ nth
#' @aliases all_ all
#' @aliases any_ any
#'
#' @title Summary functions overridden by PMDatR
#'
#' @details Some summary functions have been overridden to make them easier to use from custom function.
#'   All of these skip NA values.  If all values in x are NA, then these functions return NA, rather than
#'   other values such as -Inf, Inf, or NaN.
#'
#'   In the event of a tie (i.e. multiple modes) mode_ returns the first mode encountered in x, or if
#'   tie.order is provided then the first in the list.
#'
#' @param x An R object with a mean method (e.g. numeric/logical vectors)
#' @param n For 'nth' the selector to use.  Note that NAs are skipped.
#' @param ... Additional arguments passed to or from other methods
#' @param na.rm TRUE (the default) if NA values should be ignored in the calculation
#' @param tie.order NULL (the default) to return the first mode, or a character vector of the order in
#' which to choose from the ties.
#'
#' @return The value of the summary function applied to vector x
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
#' @rdname statfuns
#' @importFrom dplyr nth
#' @importFrom stats na.omit
#' @export
#' @examples
#' # with character values
#' vals = letters[c(1,1,1,2,2,1,1,1,2,3,3,2,3,3,3,2,1)]
#' mode_(vals)
#' # with factors
#' mode_(as.factor(vals))
#' # with integers
#' mode_(as.integer(as.factor(vals)))
#' # with double
#' mode_(as.double(as.factor(vals))/7)
#' # breaking ties in desired order
#' mode_(c("A","B","C","A","B","C", NA, NA), na.rm=FALSE, tie.order=c("C","A","B", NA))
mode_ <- function(x, na.rm=TRUE, tie.order=NULL){
  # find the most frequent value in x
  # be careful if x is not factor, char, or int.  numbers are converted to
  #  text by table
  # ties are broken by returning the first value in the list.
  if(na.rm){
    useNA="no"
  } else {
    useNA = "always"
  }
  val=NULL
  if(is.null(tie.order)){
    val = names(which.max(table(x, useNA = useNA)))
  } else {
    tabmax=table(x, useNA = useNA)
    maxes = which(tabmax==max(tabmax))
    #select from maxes using match
    xnams = names(maxes)
    xties = match(tie.order,xnams)
    # remove empty ties
    xties = xties[!is.na(xties)]
    if(!length(xties)){
      # no matches in ties, take first mode
      val=xnams[1]
    } else {
      val=xnams[xties[1]]
    }
  }

  if(is.null(val)) return(NA)
  if(is.numeric(x)){
    return(as.numeric(val))
  } else{
    return(val)
  }
}

#' @rdname statfuns
#' @export
any_ <- function(..., na.rm = TRUE) {
  base::any(..., na.rm = na.rm)
}

#' @rdname statfuns
#' @export
all_ <- function(..., na.rm = TRUE) {
  base::ll(..., na.rm = na.rm)
}
