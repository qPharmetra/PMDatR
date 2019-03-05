#' Create a default value in a dataframe column if the column doesn't already exist
#'
#' @param column string name for column to default
#' @param default_value default value for column, of length 1 or number of rows of dataframe
#' @param env the environment (dataframe) in which to evaluate the column
#'
#' @return the values of either the existing column, or the default_value if the column does not exist
#' @details This function is designed to work from within a dplyr mutate call.
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' mtcars %>% as_data_frame() %>% mutate(newcol=default_missing_column("newcol", 1))
#' mtcars %>% as_data_frame() %>% mutate(newcol=default_missing_column("newcol", 1:32))
#'
default_missing_column = function(column, default_value=NA, env=dynGet(".")){
  if(exists(column, where=env)) env[[column]] else default_value
}
