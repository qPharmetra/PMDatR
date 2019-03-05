
#' Lookup a value in domain data
#'
#' @param domain Quoted name of the domain data object
#' @param value_col Quoted name of the column from which to pull values
#' @param key_col Quoted name of the column to match with key_val
#' @param key_val An expression that evaluates to values to match in key_col
#'
#' @return a vector of values from value_col, of length of key_val
#'
#' @details This function allows one to lookup values in domain data.  The domain name, and the
#' two columns to use in it are given as quoted strings.  This allows the function to be used
#' within query mappings, which would otherwise error on the domain and column names that do
#' not match the query's mapped domain's columns.  The key_val is given unquoted so the expression
#' will be evaluated when the function is called.
#'
#' One particular use is to find the treatment start date for a given ID in a query.
#' @export
#'
#' @examples
#' \dontrun{
#' lookup_in_domain("DM", "RFXSTDTC", "USUBJID", ID)
#' }
lookup_in_domain = function(domain, value_col, key_col, key_val){
  # if this function is called often, it could be slow.  May need to cache the lookup table
  # as an attribute of the funciton to emulate static variable.
  tryCatch(
    {
      get(domain, envir=parent.frame())},
    error=function(cond){
      stop(sprintf("[%s] is not a valid domain.  Check spelling of domain and make sure it loaded.", domain))
      return(NULL)
    }) -> x
  misscols = setdiff(c(value_col, key_col), names(x))
  if(length(misscols)){
    stop(sprintf("column(s) [%s] not in domain [%s]", paste(misscols, collapse=", "), domain))
  }
  # we have the domain data and can select the appropriate columns
  # find first match of key_val in key_col and return value_col
  x[[value_col]][match(key_val,x[[key_col]])]
}


#' Get a column from a domain
#'
#' @param domain Quoted name of the domain data object
#' @param column Quoted name of the column from which to pull values
#' @param .unique A logical (default TRUE) stating if the return values should be reduced
#' to just unique values.
#' @param .unlist A logical (default TRUE) stating if the return value should be converted
#' an array (e.g., instead of a data_frame column)
#'
#' @return A data_frame column or array of values
#' @export
#'
#' @examples
#' \dontrun{
#' # get the unique subject ids in DM
#' get_from_domain("DM","USUBJID")
#' }
#'
get_from_domain = function(domain, column, .unique=TRUE, .unlist=TRUE){
  tryCatch(
    {
      get(domain, envir=parent.frame())
      #return(x)
    },
    error=function(cond){
      stop(sprintf("[%s] is not a valid domain.  Check spelling of domain and make sure it loaded.", domain))
      return(NULL)
    }) -> x
  if(!column %in% names(x)){
    stop(sprintf("column [%s] is not in domain [%s]", column, domain))
  }
  x=x[,column]

  if(.unique){
    x=unique(x)
  }
  if(.unlist){
    x=unlist(x, use.names = FALSE)
  }
  x
}
