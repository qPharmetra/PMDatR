

#' Compute creatinine clearance
#'
#' @param age Age in years
#' @param wt Body mass in kg
#' @param female TRUE if female, FALSE if male
#' @param creat Serum creatinine in mg/dL
#' @param method "cg" for cockroft-gault is default, and only, method.
#' @param units Desired output units.  ml/min is default and only option.
#'
#' @return vector of computed crcl values
#' @export
#'
#' @examples
#' crcl(45, 83, TRUE, .5)
crcl = function(age, wt, female, creat, method=c("cg"), units="ml/min"){
  (140-age)*wt/72/creat*ifelse(female, 0.85,1.0)
}
