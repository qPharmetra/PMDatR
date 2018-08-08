#' time_after_criteria calculates the time after some filter critera within a group
#' @param .df dataframe
#' @param .name output column name
#' @param ... filter criteria
#' @param groups grouping variables
#' @param .col column
#' @param .name_fn custom function for naming with formatting
#' @param units units time scale should be in, defaults to hours.  Possible values include: secs, mins, hours, days, weeks
#' @param digits digits to round, defaults to 2
#' @export
#' @details
#' this is a very flexible function that can be used to do almost any time_from_x calculcation.
#' For example, to given a conmed column with admininstration flagged as "Y", time from
#' conmed could be calculated simply with
#' df %>% time_after_critiera("TACONMED", conmed == "Y")
time_after_criteria <- function(.df,
                            .name,
                            ...,
                            groups = "ID",
                            .time = "TIME",
                            .name_fn = NULL,
                            units = "hours",
                            digits = 2) {
  .col <- .time
  possible_units = c("secs" = 1, "mins" = 60, "hours" = 3600,
                     "days" = 86400, "weeks" = 604800)
  if (!units %in% names(possible_units)) {
    warning("not one of the possible unit conversions, defaulting to hours")
    units <- "hours"
  }
  unit_sf <- possible_units[[units]]
  if (!is.null(groups)) {
    .df <- group_by_(.df, .dots = groups)
  }

  .df <- .df %>%
    filter(...) %>%
    mutate(FLG__ = 1) %>%
    left_join(.df, .) %>%
    mutate(
      FLGNUM__ = cumsum(ifelse(is.na(FLG__), 0, FLG__))
           ) %>%
    group_by(FLGNUM__, add = TRUE)
  output_list <- diff_col(.df, .col, as.character(unlist(dplyr::groups(.df))), .name = .name, .name_fn = .name_fn)
  output <- output_list$df %>%
    mutate_(.dots = setNames(list(
    lazyeval::interp(~ round(as.numeric(.diffcol)/unit_sf, digits),
                     .diffcol = as.name(output_list$.diff_name)
    ),
    # TODO: any values in grp 0 should be set to 0 for now until we decide a canonical way
    # of handling times prior to the filter criteria
    lazyeval::interp(~ ifelse(FLGNUM__ == 0, 0, .diffcol),
                     .diffcol = as.name(output_list$.diff_name)
    )
    ), c(output_list$.diff_name, output_list$.diff_name))) %>% select(-FLG__, -FLGNUM__, -DIFF__)
  return(output)
}

