#' spread that understands units
#' @param data A data frame.
#' @param key The bare (unquoted) name of a column whose values will be used as column headings
#' @param value The bare (unquoted) name of a column whose values will populate the cells
#' @param ... parameters to pass to the underlying tidyr::spread function
#' @param .units_col column associated with units in the value column
#' @param .units_key a list of column names and units to attempt to set
#' @param .col_precendence whether units found in the units_col take precedence over those found in the key
#' @export
spread_ <- function(data, key, value, ..., .units_col = NULL, .units_key = NULL, .col_precedence = TRUE) {
  if (is.null(.units_col) && is.null(.units_key)) {
    return(tidyr::spread_(data, key, value, ...))
  }
  key_list <- list()
  if (!is.null(.units_col)) {
    units_col_key <- dplyr::distinct_(data, .dots = c(key, .units_col))
    if (dplyr::n_distinct(units_col_key[[key]]) != nrow(units_col_key)) {
      warning("multiple units detected for a single key, only using the first one!")
      units_col_key <- dplyr::distinct_(units_col_key, .dots = key, .keep_all = TRUE)
    }
    # get rid of the key column, else will result in duplicate rows after spread with NA's
    # all over the place, eg parent metabolite stacked when spread would alternate having
    # NA for parent/metabolite column given that the units column would exist, expecting a row
    # with data about each unit
    data <- data %>% dplyr::select_(.dots = lazyeval::interp(~-unit, unit = as.name(.units_col)))
    key_list <- as.list(setNames(units_col_key[[.units_col]], units_col_key[[key]]))
  }
  if (!is.null(.units_key)) {
    # if have keys from units_col then need to resolve column precedence too
    if (length(key_list) && .col_precedence) {
      final_list <- as.list(resolve_units(list(unlist(key_list), unlist(.units_key))))
    } else {
      final_list <- as.list(resolve_units(list(unlist(.units_key), unlist(key_list))))
    }
  } else {
    final_list <- key_list
  }
  return(set_units_from_list(tidyr::spread_(data, key, value, ...), final_list))
}
