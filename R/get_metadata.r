

#' Gather metadata from a data frame
#'
#' @param df An object derived from data.frame
#'
#' @return A metadata S3 object
#' @export
#'

get_metadata = function(df)
{

  #capture the dataframe columns, classes, uniques

  if(!"data.frame" %in% class(df)) stop("get_metadata argument 'dom' must have member $Data as data.frame object")
  dfname = deparse(substitute(df))
  cols=colnames(df)

  ## Get metadata from data.frame column
  getColumnData = function(x, i){
    #x is dataframe, i is column name
    l = list()
    l$name = i
    if(!is.null(attributes(x[[i]])$type)){
      l$storage=attributes(x[[i]])$type
    } else{ #guess types based on intrisic type
      l$storage = switch(class(x[[i]])[1],
                               integer=,
                               numeric="Numeric",
                               POSIXct="DateTime",
                               logical="Logical",
                               ordered=,
                               factor="Categorical",
                               character="Text",
                               "Unknown")
    }


    if(any(c("Numeric","DateTime") %in% l$storage)){
      l$minimum = suppressWarnings(as.character(min_(x[[i]], na.rm=T)))
      l$maximum = suppressWarnings(as.character(max_(x[[i]], na.rm=T)))
      l$unique = as.list(NA)
    } else {

      l$minimum = NA
      l$maximum = NA
      # we'll need to fix this so we don't pick up all uniques for non-factor columns.  If pulling from DataColumn class
      # we can use the storage class there to decide this.
      l$unique = unique(x[[i]])
      if(l$storage %in% c("Text","Unknown")) l$unique = head(l$unique,5)
      l$unique=as.list(l$unique)
    }
    l
  }
  colmeta=lapply(names(df),getColumnData,x=df)
  if(is.null(colmeta)) colmeta=list()

  structure(colmeta,
    #names = names(df), # don't name list elements or these won't load as a vector of objects in C#
    class = "domain.metadata"
  )

}

reformat_metadata=function(cols){
  # puts the unique back into list format
  # meta is otherwise correctly formatted metadata
  if(length(cols)>0){
    for(i in 1:length(cols)){
      cols[[i]][["unique"]]=as.list(unlist(cols[[i]][["unique"]]))
    }
  }
  cols
}

#' Format a metadata object as a dataframe
#'
#' @param met A metadata object
#' @param type md for markddown formatting of multiline cells
#'
#' @return a data_frame (tbl_df) object
#' @export
#' @method as.data.frame domain.metadata
#'

as.data.frame.domain.metadata = function(met, type="md"){
  # print columns with Name, Type, Range (min -- max, or multiline uniques)
  #check met is metadata class
  df = data_frame(Column=character(), Type=character(), Range=character())
  keepnames = names(df)
  for(col in met){
    if(!is.na(col$unique[1])){
      padding = "" # paste(rep(" \\\n", length(col$unique)-1), collapse="")
      sumry = paste0(col$unique, collapse="\\\n")
      row = c(Column=sprintf("%s%s", col$name, padding),
                 Type=sprintf("%s%s", col$storage, padding),
                 Values=sumry)
    } else{
      row = c(Column=col$name,
                 Type=col$storage,
                 Values=sprintf("%s -- %s", col$minimum, col$maximum))
    }
    df = rbind(df, row)
  }
  names(df) = keepnames
  df
}
