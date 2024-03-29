
#' Applies Box-Cox transformation with known lambda
#'
#' @param x Values to transform
#' @param lambda Power parameter
#'
#' @return the transformed values.
#' @export
#'
#' @examples
#' boxcox(c(1,2,3))
#' boxcox(1:3, lambda=1)
boxcox = function(x, lambda=0){
  if(any(x<0 & lambda<1)) warning("Unsafe values sent to Box-Cox transformation")
  y=ifelse(lambda==0, log(x), (x^lambda-1)/lambda)
  if(any(is.nan(y))) warning("NaN values generated by Box-Cox transformation")
}

#' Applies a power transform y=a*x^b
#'
#' @param x Values to transform
#' @param a The scalar parameter
#' @param b The power parameter
#'
#' @return the transformed values
#' @export
#'
#' @examples
#' power(1:3, a=10, b=.5)
power = function(x, a=1, b=1){
  a*x^b
}

