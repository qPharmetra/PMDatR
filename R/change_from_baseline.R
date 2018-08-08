#' absolute change from baseline
#' @param .df data frame
#' @param .col column name (unquoted)
#' @param groups vector of groups, defaults to "ID"
#' @param ... filter functions (unquoted) to establiash baseline row, defaults to first row per group
#' @param .name name of created column (quoted).  Defaults to CFB_<.col>.
#' @param .name_fn function to apply formatting to the given colun name
#' @param .agg_fn function to calculate an aggregate statistic for the baseline value
#' @importFrom dplyr summarise_
#' @export
cfb_abs <- function(.df,
                    .col,
                    ...,
                    groups = "ID",
                    .name = NULL,
                    .name_fn = function(x) {paste0("CFB_", x)},
                    .agg_fn = NULL) {
  .col <- deparse(substitute(.col))
  output_list <- diff_col(.df, .col, groups, ..., .name = .name, .name_fn = .name_fn, .agg_fn = .agg_fn)
  output_list$df %>% select(-DIFF__)
}

#' ratio change from baseline
#' @param .df data frame
#' @param .col column name (unquoted)
#' @param groups vector of groups, defaults to "ID"
#' @param ... filter functions (unquoted) to establiash baseline row, defaults to first row per group
#' @param .name name of created column (quoted).  Defaults to CFBR_<.col>.
#' @param .name_fn function to apply formatting to the given colun name
#' @param digits number of digits to round final value to
#' @param .agg_fn function to calculate an aggregate statistic for the baseline value
#' @export
cfb_ratio <- function(.df,
                    .col,
                    ...,
                    groups = "ID",
                    .name = NULL,
                    .name_fn = function(x) {paste0("CFBR_", x)},
                    digits = 3,
                    .agg_fn = NULL) {
  .col <- deparse(substitute(.col))
  output_list <- diff_col(.df, .col, groups, ..., .name = .name, .name_fn = .name_fn, .agg_fn = NULL)
  output_list$df %>%
    mutate_(.dots = setNames(list(
      lazyeval::interp(~ round(.diffcol/DIFF__, digits),
                         .diffcol = as.name(output_list$.diff_name)
                        )), output_list$.diff_name)) %>% select(-DIFF__)
}

#' percent change from baseline
#' @param .df data frame
#' @param .col column name (unquoted)
#' @param groups vector of groups, defaults to "ID"
#' @param ... filter functions (unquoted) to establiash baseline row, defaults to first row per group
#' @param .name name of created column (quoted).  Defaults to CFBP_<.col>.
#' @param .name_fn function to apply formatting to the given colun name
#' @param digits number of digits to round result
#' @param .agg_fn function to calculate an aggregate statistic for the baseline value
#' @export
cfb_percent <- function(.df,
                      .col,
                      ...,
                      groups = "ID",
                      .name = NULL,
                      .name_fn = function(x) {paste0("CFBP_", x)},
                      digits = 1,
                      .agg_fn = NULL) {
  .col <- deparse(substitute(.col))
  output_list <- diff_col(.df, .col, groups, ..., .name = .name, .name_fn = .name_fn, .agg_fn = NULL)
  output_list$df %>%
    mutate_(.dots = setNames(list(
      lazyeval::interp(~ round(.diffcol/DIFF__*100, digits),
                       .diffcol = as.name(output_list$.diff_name)
    )), output_list$.diff_name)) %>% select(-DIFF__)
}

#' Find a baseline value
#'
#' @details get_baseline can be used inside a mutate or summarise call, and therefore inside the get mapped
#'   value functions (e.g., \code{getDV}). It finds the rows that satisfy the criteria in the .filter argument, then applies fun.summ
#'   (a summary function) to those rows.  In this way, multiple values can be used for a baseline value.
#'
#' @param col An unquoted variable name
#' @param .filter An unquoted R logical expression (evaluates to TRUE or FALSE).  Pass TRUE to select
#' all records for summarization.
#' @param ... Comma separated, unquoted, additional grouping columns
#' @param fun.summ An unquoted function name to use to summarise multiple baseline values per group
#'
#' @return Returns a vector of selected baseline values
#'
#' @importFrom tidyr unite
#' @export
#'
#' @examples
#' library(dplyr)
#' library(PMDatR)
#' Theoph %>% mutate(conc.bl = get_baseline(conc, Time>0 & Time<1, Subject, fun.summ=mean_))
#' Theoph %>% mutate(conc=set_units(conc,"mg/ml"),
#'   conc.bl = get_baseline(conc, Time>0 & Time<1, Subject, fun.summ=mean_)) %>% as_data_frame
get_baseline = function(col, .filter, ..., fun.summ=mean_){
  my.df = bind_cols(data_frame(.cond=.filter, val=col), as.data.frame(list(...)) %>% tidyr::unite(.group, everything()))
  .bl= my.df[.filter,] %>%
    #filter() %>%
    group_by(.group) %>%
    dplyr::summarise_at("val",funs(fun.summ))
  .bl=left_join(my.df, .bl, by=".group")$val.y
  set_units(.bl,units(my.df$val))
}
