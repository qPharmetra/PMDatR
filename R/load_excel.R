

#' Load an Excel sheet from a workbook
#'
#' @param obj A domain object.
#' @details  Reads a file in xls or xlsx format and casts it as a tbl_df (dplyr) object.  It must be passed a valid
#' domain object, that may also contain an xlSettings object.  For full description of xlSettings options see the arguments list for
#' \link[readxl]{read_excel}
#'
#' @return A tbl_df object containing the loaded data
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr as_data_frame
#' @importFrom tidyr gather_
#'


load_excel = function(obj){
  #obj is a domain settings object (named list)
  # use "header" as col_names
  if(!is.empty.yaml(obj$FileSettings$header)) obj$FileSettings$col_names=obj$FileSettings$header
  if(is.empty.yaml(obj$FileSettings$range)) obj$FileSettings$range=NULL
  if(is.empty.yaml(obj$FileSettings$sheet)) obj$FileSettings$sheet=NULL
  if(is.empty.yaml(obj$FileSettings$skip)) obj$FileSettings$skip=NULL
  allowed.args = names(obj$FileSettings) %in% names(formals(readxl::read_excel))
  df = dplyr::as_data_frame(do.call(readxl::read_excel,
                                    c(list(path=obj$filepath),obj$FileSettings[allowed.args])))
  # if we're asked to reshape
  if(!is.empty.yaml(obj$FileSettings$id_cols)){
    # id_cols will be a single string, so parse it
    id_cols=unlist(strsplit(obj$FileSettings$id_cols, "\\s*,\\s*"))
    gather_cols = names(df)[!names(df) %in% id_cols]
    df = tidyr::gather_(df, "Key", "Value", gather_cols, convert=T)
  }
  df
}
