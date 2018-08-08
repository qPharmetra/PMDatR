#' expand rows that correspond to addl records
#' @param .df dataframe to process
#' @param ii_scale scaling factor to expand addl columns by. Defaults to 3600
#' @param time_related_cols column name and scaling factor for time-dependent columns
#' @importFrom dplyr arrange bind_rows
#' @details
#' scaling factor needed to figure out units to increment the time column by, based
#' on the TIME column. Given time in date-time format, default unit of incrementation is seconds,
#' so the ii_scale should be 3600 for II given in hours.
#'
#' Given a Time in hours and II in hours, the ii_scale would be 1.
#'
#'
#' time_related_cols should be a named vector, where the name corresponds to the column and
#' the value is the scaling factor to scale the increment column by. Eg, given II records in days
#' and an additional column AGEYRS, while expanding addl records, this must also
#' be appropriately incremented in year increments. In the case of having a TIME
#' variable in days, the time_related_cols value represents the scaling factor
#' so time_related_cols = c("AGEYRS" = 1/365)
#' @importFrom dplyr filter mutate
#' @importFrom purrr by_row map_df
#' @importFrom tidyr unnest
#' @export
expand_addl <- function(.df, ii_scale = 3600, time_related_cols = NULL) {
  addl_df <- .df %>% filter(ADDL > 0)
  if (!nrow(addl_df)) {
    return(.df)
  }

  expanded_addl <- by_row(addl_df, function(row) {
    naddl <- row$ADDL
    expanded_df <- map_df(1:naddl, function(i) {
      # setting all created rows an ADDL value of -1 to distinguish
      incremented_df <- row %>% mutate(ADDL = -1, TIME = TIME + i*II*ii_scale)

      # construct an expression list
      if (!is.null(time_related_cols)) {
       # TODO: check all columns actually exist

        # the purpose of this is to create expressions that will allow for a single
        # mutate call to modify all time related columns, given quoted names. This
        #  call will take, for example "AGEYRS" = 1/365
        # and return an expression ~ AGEYRS + II*0.0027397, which the SE versions
        # of dplyr functions, such as mutate_ will understand.
        dots <- lapply(names(time_related_cols), function(cn) {
          scaling_factor <- time_related_cols[[cn]]
          lazyeval::interp(~ cn + II*sf, cn = as.name(cn), sf = scaling_factor)
        })
        incremented_df <- incremented_df %>% dplyr::mutate_(.dots = setNames(dots, names(time_related_cols)))
      }
      return(incremented_df)
    })
    # by design leaving the original ADDL values in and not setting everything
    # to zero, so can distinguish addl rows and derived rows
    return(expanded_df)
  }, .collate = "list", .labels = FALSE) %>%
    unnest()
  # collating by list and then unnesting is the strategy that
  # seems to maintain the dttime nature of TIME, however, when
  # collating to row causes a coercion to dbl.

  # probably should refactor the explicit ID/TIME sorting vars to be more flexible
  results <- arrange(
    bind_rows(
    .df,
    expanded_addl
  ), ID, TIME)
  return(results)
}
