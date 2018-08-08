#' bind rows for dataframes with units
#' @param ... dataframes to bind together
#' @importFrom purrr map map_lgl
#' @examples
#' library(dplyr)
#' library(PMDatR)
#' th1 <- set_units_from_list(Theoph, list(Wt = "kg")) %>%
#'     mutate(WTunit="kg")
#' th2 <- set_units_from_list(Theoph, list(Wt = "lb")) %>%
#'     mutate(WTunit = "lb")
#'
#' #the WTFLG groups are both scaled to kg
#' bind_rows(th1, th2) %>%
#'     distinct(Subject, WTunit, .keep_all = TRUE)
#' @export
bind_rows <- function(..., .id=NULL) {
  list_df <- list(...)
  cols_w_units <- map(list_df, unit_cols)
  if(!any(map_lgl(cols_w_units,~ !is.null(.x)))) {
    ## convert data_frame columns, if necessary
    list_df = harmonize_for_bind_rows(list_df)
    return(dplyr::bind_rows(list_df, .id))
  }
  common_units <- resolve_units(cols_w_units)

  ## convert data_frame columns, if necessary
  list_df = harmonize_for_bind_rows(list_df)
  # bind together all dataframes, while also converting
  # all columns to be consistent
  # this will remove all units as bind_rows in dplyr 0.5 chokes
  # on the unknown attributes
  output <- purrr::map_df(list_df, function(.df) {
    strip_units_df(convert_units_from_list(.df, as.list(common_units)))
  })
  # re-set units to common outputs
  set_units_from_list(output, as.list(common_units)) %>% as.tbl
}


## utility functions for bind rows

# build a list of column types

get_column_types = function(list_df){
  get_class = function(x){
    if(inherits(x,"factor")) return("factor")
    if(inherits(x,"character")) return("character")
    if(inherits(x,c("numeric","double","integer"))) return("numeric")
    if(inherits(x,"logical")) return("logical")

  }

  df_types = lapply(list_df, function(x) lapply(x, get_class))
  # get unique column names
  df_names = unique(unlist(lapply(df_types, names)))
  cols = setNames(vector("list",length(df_names)), df_names)
  for(l in df_types){
    for(n in names(l)) cols[[n]] =  c(cols[[n]], l[[n]])
  }

  cols
}

# figure out out if a conversion is needed
get_convert_type = function(types){
  # given a vector of types, find the most specific type that all can be converted to
#   ## FIRST, make sure we only allow PMDatR supported types
#   if(!all(types %in% c("logical", "integer", "double", "character")))

#   # only one type of value, no conversion needed
  if(length(unique(types))==1) return(NULL)
  # if any are character or factor, convert to character
  if(any(types %in% c("character","factor"))) return("as.character")
  # there's a mix of logical, don't let it convert to numeric
  if(any(types %in% "logical")) return("as.character")
  return(NULL)
}

# convert columns in data frames, as needed ### NOTE: THIS BREAKS IF WE EVER WANT TO CONVERT TO NON_CHAR
harmonize_for_bind_rows = function(list_df){
  cols = get_column_types(list_df)
  cols = lapply(cols, get_convert_type)
  cols = cols[!sapply(cols,is.null)]
  # if there were any columns needing harmonization, we'll take care of them now
  #local helper function to channel the conversion by each df
  convert_data_frame = function(df,conv_cols){
    df %>% purrr::dmap_at(names(conv_cols),as.character)
  }
  # if there are any cols to mutate, lapply through the dataframes and return the list
  if(length(cols)>0) list_df = lapply(list_df, convert_data_frame, cols)
  list_df
}

