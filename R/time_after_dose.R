#' time after the first dose
#' @param .df dataframe
#' @param groups grouping variables
#' @param .name output column name
#' @param .criteria a logical expression to identify relavent doses.  Defaults to EVID equal 1 or 4 (dose records).
#' @param .name_fn custom function for naming with formatting
#' @param .time time column name, defaults to TIME
#' @param units units time scale should be in, defaults to hours
#' @param digits digits to round, defaults to 2
#' @export
time_after_first_dose <- function(.df,
                                  groups = "ID",
                                  .name = "TAFD",
                                  .criteria = EVID %in% c(1,4),
                                  .name_fn = NULL,
                                  .time = "TIME",
                                  units = "hours",
                                  digits = 10) {
  .col <- .time
  possible_units = c("secs" = 1, "mins" = 60, "hours" = 3600,
                     "days" = 86400, "weeks" = 604800)
  if (!units %in% names(possible_units)) {
    warning("not one of the possible unit conversions, defaulting to hours")
    units <- "hours"
  }
  unit_sf <- possible_units[[units]]
  output_list <- diff_col(.df, .col, groups, .criteria, .name = .name, .name_fn = .name_fn)
  output_list$df %>% mutate_(.dots = setNames(list(
    lazyeval::interp(~ round(as.numeric(.diffcol)/unit_sf, digits),
                     .diffcol = as.name(output_list$.diff_name)
    )), output_list$.diff_name)) %>% select(-DIFF__)
}

#' time after dose
#' @param .df dataframe
#' @param groups grouping variables
#' @param .name output column name
#' @param .criteria a logical expression to identify relavent doses.  Defaults to EVID equal 1 or 4 (dose records).
#' @param .name_fn custom function for naming with formatting
#' @param .time time column name, defaults to TIME
#' @param units units time scale should be in, defaults to hours
#' @param digits digits to round, defaults to 2
#' @export
time_after_dose <- function(.df,
                            groups = "ID",
                            .name = "TAD",
                            .criteria=EVID %in% c(1,4),
                            .name_fn = NULL,
                            .time = "TIME",
                            units = "hours",
                            digits = 10) {
  .col <- .time
  possible_units = c("secs" = 1, "mins" = 60, "hours" = 3600,
                     "days" = 86400, "weeks" = 604800)
  if (!units %in% names(possible_units)) {
    warning("not one of the possible unit conversions, defaulting to hours")
    units <- "hours"
  }
  unit_sf <- possible_units[[units]]

  if ("ADDL" %in% names(.df)) {
    .df <- .df %>% expand_addl
  }
  if (!is.null(groups)) {
    .df <- group_by_(.df, .dots = groups)
  }
  #.df <- .df %>% mutate(DOSENUM__ = cumsum(ifelse(AMT > 0 , 1, 0))) %>%
  .df <- .df %>% mutate_(.dots=setNames(list(lazyeval::interp(~cumsum(crit),
                                             crit=lazy(.criteria))),"DOSENUM__")) %>%
    group_by(DOSENUM__, add = TRUE)
  output_list <- diff_col(.df, .col, as.character(unlist(dplyr::groups(.df))), .name = .name, .name_fn = .name_fn)
  output <- output_list$df %>% mutate_(.dots = setNames(list(
    lazyeval::interp(~ round(as.numeric(.diffcol)/unit_sf, digits),
                     .diffcol = as.name(output_list$.diff_name)
    )), output_list$.diff_name)) %>% select(-DIFF__)
  if ("ADDL" %in% names(.df)) {
    # collapse back down
    output <- output %>% filter(ADDL != -1)
  }
  return(output)
}


#' relative time such that first record is at time 0
#' @param .df dataframe
#' @param groups grouping variables
#' @param .name output column name
#' @param .name_fn custom function for naming with formatting
#' @param .time time column name, defaults to TIME
#' @param units units time scale should be in, defaults to hours
#' @param digits digits to round, defaults to 2
#' @export
relative_time <- function(.df,
                      groups = "ID",
                      .name = "RTIME",
                      .name_fn = NULL,
                      .time = "TIME",
                      units = "hours",
                      digits = 10) {
  .col <- .time
  possible_units = c("secs" = 1, "mins" = 60, "hours" = 3600,
            "days" = 86400, "weeks" = 604800)
  if (!units %in% names(possible_units)) {
    warning("not one of the possible unit conversions, defaulting to hours")
    units <- "hours"
  }
  unit_sf <- possible_units[[units]]
  output_list <- diff_col(.df, .col, groups, .name = .name, .name_fn = .name_fn)
  output_list$df %>% mutate_(.dots = setNames(list(
    lazyeval::interp(~ round(as.numeric(.diffcol)/unit_sf, digits),
                     .diffcol = as.name(output_list$.diff_name)
    )), output_list$.diff_name)) %>% select(-DIFF__)
}

#' Time of the first dose
#' @details This functions operates on the entire dataframe given by .df and finds the first time specified in
#'   .time that matches the expression passed in .critera (by default it looks for EVID 1 or 4).  The value is returned in
#'   a newly created column specified by .name.
#' @param .df dataframe
#' @param .criteria An expression that identifies valid dose records
#' @param .name output column name as a string
#' @param .time time column name, as a string. Defaults to TIME
#' @importFrom lazyeval lazy lazy_dots
#' @export
time_of_first_dose <- function(.df,
                               .name = "FDDTTM",
                               .criteria=EVID %in% c(1,4),
                               .time = "TIME") {

  dots = setNames(list(lazy(.criteria)),"CRIT__")
  add.col = setNames(list(interp(~ timename[which(CRIT__==TRUE)[1]],
                                 timename = as.name(.time))), .name)
  .df %>% mutate_(.dots=dots) %>% mutate_(.dots=add.col) %>% select(-CRIT__)

}

#time_after_first_dose(output)
#two_ids <- bind_rows(output, output %>% mutate(TIME = TIME + 6000000, ID = 2))
#two_ids %>% time_after_dose()
#
#two_ids %>% relative_time()
#two_ids %>% relative_time("TIME", groups = NULL)
#
#two_ids %>% time_after_dose()
#two_ids %>% time_after_dose("TIME", groups = NULL)
