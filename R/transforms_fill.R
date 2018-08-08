#utility
#' Generate a character vector from unquoted names
#'
#' @details This function is particularly useful when typing, for instance, a list of column names.
#' \code{.(ID,TIME)} is the same as \code{c("ID","TIME")}
#' @param ... names to turn into text and place in a character vector
#'
#' @return A character vector of names
#' @export
#'
#' @examples
#' .(this,is,how,it,works)
#'
`.` <- function (...)
{
  as.character(match.call()[-1])
}

#Fill functions

#' Fill NA values in columns
#' @rdname fill
#' @details The fill_* functions update NA values in a data.frame column.
#' \code{fill_NA} updates all NA values with the supplied value or expression
#' \code{fill_locf} fills NA forward unless the difference in \code{time} exceeds \code{tolerance}.
#' \code{fill_nocb} fills NA backward unless the difference in \code{time} exceeds \code{tolerance}.
#' Both locf and nocb fills fill to the start and end (respectively) of the column.  That is, for locf, if the
#' first values are NA, they are filled back from the next non-NA value.  Similare behavior is applied at the end of
#' column in the case of nocb, when the final values are NA.
#'
#' @param .data A data.frame in which to modify the column
#' @param column The column to modify as an unquoted name
#' @param value A value or expression to use in replacing NA values
#' @param groups A character vector of group column names
#'
#' @return the modified data.frame
#' @importFrom lazyeval expr_text
#' @importFrom dplyr "%>%" group_by_ mutate_
#' @importFrom stats setNames as.formula
#' @export
#'

fill_NA = function(.data, column, value, groups="ID"){
  col = expr_text(column)
  val = expr_text(value)
  fn = as.formula(sprintf("~ifelse(is.na(%s),%s,%s)",col, val, col))
  #sets column to value when column is .na
  .data %>% group_by_(.dots=groups) %>%
    mutate_(.dots=setNames(list(fn),col))
}

#' @rdname fill
#' @param tolerance A numeric value for the tolerance of the supplied time column.  Defaults to Inf.
#' @param time The column in .data (as a quoted string) that is used to determine if too much time has passed to carry
#' values.
#' @param backfill A logical value indicating whether NA values at the beginning of the vector
#' should be filled backward from the next value. (for fill_nocb, fills terminal NA values from last
#' value.)  Defaults to TRUE.
#' @importFrom lazyeval expr_text
#' @importFrom dplyr "%>%" group_by_ mutate_
#' @importFrom stats setNames as.formula
#' @export
fill_locf = function(.data, column, groups="ID", tolerance=Inf, time="TIME", backfill=T){
  col = expr_text(column)
  #
  fn = as.formula(sprintf("~locf(%s, cond=(%s-lag(%s))<=%s, backfill=%s)",col, time, time, tolerance, backfill))
  #sets column to value when column is .na
  .data %>% group_by_(.dots=groups) %>%
    mutate_(.dots=setNames(list(fn),col))
}

#' @rdname fill
#' @importFrom lazyeval expr_text
#' @importFrom dplyr "%>%" group_by_ mutate_
#' @importFrom stats setNames as.formula
#' @export
fill_nocb = function(.data, column, groups="ID", tolerance=Inf, time="TIME", backfill=T){
  col = expr_text(column)
  #
  fn = as.formula(sprintf("~nocb(%s, cond=lag(%s)<=%s, backfill=%s)",col, time, tolerance, backfill))
  #sets column to value when column is .na
  .data %>% group_by_(.dots=groups) %>%
    mutate_(.dots=setNames(list(fn),col))
}

#' Add new variables to a data_frame
#'
#' @details This function provides a wrapper around \code{dplyr::mutate}.  It implements the \code{base::transform}
#' generic for data_frame objects.
#' @param .data The data_frame object to apply the transformation to
#' @param ... Name-value pairs of expressions. Use NULL to drop a variable.
#'
#' @return A copy of .data with the transformed columns
#' @importFrom dplyr mutate
#' @export
#'
#' @examples
#' library(dplyr)
#' CO2 %>% as_data_frame %>% transform(newType=Type)
transform.tbl_df = function(.data,...){ dplyr::mutate(.data, ...)}

#' Recode/Replace values in a data frame column
#'
#' @param .data A data_frame type object
#' @param .col The unquoted name of the column to recode
#' @param ... Name-value pairs of expressions to use in replacement
#' @param .not_found If a value in .col is not in ... , the value to it replace with.
#'
#' @details This function is used to change values in a column.  The name in the ... argument
#' can be a string or a quoted value.  See the examples.
#'
#' @return A copy of .data with the modified column
#' @importFrom  stats as.formula
#' @importFrom lazyeval lazy_dots expr_text
#' @export
#'
#' @examples
#' library(dplyr)
#' data_frame(ID=1:3,SEX=c("M","F","X")) %>% recode_values(SEX, "M"=1, "F"=2)
#' data_frame(ID=1:3,SEX=c("M","F","X")) %>% recode_values(SEX, "M"="Male", "F"="Female")
#' data_frame(ID=1:3,SEX=c("M","F","X")) %>% recode_values(ID, "1"=1101, "2"=1102)
#' data_frame(ID=1:3,SEX=c("M","F","X")) %>% recode_values(ID, "1"=1101, "2"=1102, .not_found=NA)
#' data_frame(ID=1:3,SEX=c("M","F","X")) %>% recode_values(ID, "1"=1101, "2"=1102, "4"="not here")
recode_values = function(.data, .col,  ..., .not_found){

  dots = sapply(match.call(expand.dots = T)[c(-1,-2,-3)], deparse)
  dot_names = names(dots)
  ldots = lapply(seq_along(dots), function(x) sprintf("`%s`=%s",dot_names[x],dots[x]))
  .dots=paste(ldots,collapse = ", ")

  col = expr_text(.col)
  #
  fn = as.formula(sprintf("~replace_values(%s, %s)",
                          col, .dots))
  #sets column to value when column is .na
  .data %>%
    mutate_(.dots=setNames(list(fn),col))
}

#' Create an exclusion column
#'
#' @param .data A data_frame type object
#' @param .col The unquoted name of the column to create
#' @param ... Name-value pairs of conditions to use in creating codes
#' @param method one of "first" or "last", determining which code to use in
#' case multiple conditions are true
#' @param default The value to use if no conditions are true.  Defaults to "OK".
#'
#' @return the modified .data data_frame
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' library(dplyr)
#' data_frame(ID=1:3,SEX=c("M","F","X")) %>%exclusion_codes(EXCL, BADSEX=SEX=="X", remove.subject=ID==3)
#' data_frame(ID=1:3,SEX=c("M","F","X")) %>%exclusion_codes(EXCL, BADSEX=SEX=="X", remove.subject=ID==2)
exclusion_codes = function(.data, .col , ...,  method="first", default="OK"){
  .dots = lazy_dots(...)
  dots_to_string = function(.dots){
    dot_names = names(.dots)
    ldots = lapply(seq_along(.dots), function(x) sprintf("%s=%s",dot_names[x],deparse(.dots[[x]]$expr)))
    paste(ldots,collapse = ", ")
  }
  .dots=dots_to_string(.dots)

  col = expr_text(.col)
  #
  fn = as.formula(sprintf("~conditional_values( %s, method=%s, default=%s)",
                          .dots, expr_text(method), expr_text(default)))
  #sets column to value when column is .na
  .data %>%
    mutate_(.dots=setNames(list(fn),col))
}
