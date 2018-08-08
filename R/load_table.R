

#' Load a delimited text file
#'
#' @param obj A domain object.
#' @details Read a file in table format and casts it as a tbl_df (dplyr) object.  It must be passed a valid
#' domain object, that may also contain a FileSettings object.  The FileSettings object must then contain at least one
#' named item to specify the seperator (sep). If FileSettings is missing default options are used per \link[utils]{read.csv}.
#' For full description of FileSettings options see the arguments list for
#' \link[utils]{read.table}
#'
#' @return A tbl_df object containing the loaded data
#' @export
#' @importFrom dplyr as_data_frame
#' @importFrom tidyr gather_
#'


load_table = function(obj){
  #obj is a domain settings object (named list)

  #FileSettings must be present in obj and must contain at least sep
  if(is.null(obj$FileSettings$sep)){
    obj$FileSettings$sep="comma"
    obj$FileSettings$header=T
    obj$FileSettings$quote="double_quote"
    obj$FileSettings$fill=T
    obj$FileSettings$comment.char=""
    warning(paste0("Domain: ",obj$name,
                " - Using default settings for read.csv"))
  }

  #fix sep and quote
  if(!is.null(obj$FileSettings$sep)) obj$FileSettings$sep = get_sep_char(obj$FileSettings$sep)
  if(!is.null(obj$FileSettings$quote)) obj$FileSettings$quote = get_quote_char(obj$FileSettings$quote)


  allowed.args = names(obj$FileSettings) %in% names(formals(read.table))
  df = dplyr::as_data_frame(do.call(read.table, c(list(file=obj$filepath),obj$FileSettings[allowed.args])))

  # if we're asked to reshape
  if(!is.empty.yaml(obj$FileSettings$id_cols)){
    # id_cols will be a single string, so parse it
    id_cols=unlist(strsplit(obj$FileSettings$id_cols, "\\s*,\\s*"))
    gather_cols = names(df)[!names(df) %in% id_cols]
    df = tidyr::gather_(df, "Key", "Value", gather_cols, convert=T)
  }
  df
}

get_sep_char = function(sep_name){
  seps = list(whitespace="", tab="\t", comma=",", semicolon=";")
  ret = seps[[sep_name]]
  if(is.null(ret)) ret=sep_name #no translation available, take it literally
  ret
}

get_quote_char = function(quote_name){
  quotes = list(none="", single_quote="'", double_quote="\"")
  ret = quotes[[quote_name]]
  if(is.null(ret)) ret=quote_name #no translation available, take it literally
  ret
}
