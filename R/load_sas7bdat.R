

#' Load a sas7bdat file into a domain object
#'
#' @param path The fully qualified filename

#'
#' @return A tbl_df object containing the loaded data
#' @export
#' @import haven
#'

load_sas7bdat = function(path){
  haven::read_sas(path)
}
