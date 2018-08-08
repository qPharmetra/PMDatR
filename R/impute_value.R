#' impute by a calculated value for any NA's in a column
# impute_fn <- function(.df, .x, .fn = function(.x) {mean(.x, na.rm = TRUE)}, ...) {
#   .col <- deparse(substitute(.x))
#   .df %>%
#     mutate_(.dots = setNames(list(
#       lazyeval::interp( ~ ifelse(is.na(.col), .fn(), .col), .col = as.name(.col))
#     ), .col))
# }
#
#
# tmp <- data_frame(ID = 1, val = c(1, NA, 3))
#
# impute_value(tmp, val)

#' impute a value to all missing
#' @param .df data frame
#' @param .col column
#' @param .value value
#' @export
impute_value <- function(.df, .col, .value) {
  .coltext <- deparse(substitute(.col))
  .df %>% mutate_(.dots = setNames(list(
    lazyeval::interp(~ ifelse(is.na(.colt), .value, .colt),
                     .colt = as.name(.coltext))), .coltext))
}

#' impute a value to all missing
#' @param .df data frame
#' @param .col column
#' @param .fn impute function, provides column as x
#' @param groups groups, defaults to ID
#' @export
impute_fn <- function(.df, .col, .fn, groups = "ID") {
  .coltext <- deparse(substitute(.col))
  .df %>% mutate_(.dots = setNames(list(
    lazyeval::interp(~ .fn(.colt),
                     .colt = as.name(.coltext)
                     ),
    lazyeval::interp(~ ifelse(is.na(.colt), IVALS__, .colt),
                     .colt = as.name(.coltext)
                     )
    ), c("IVALS__", .coltext))) %>% select(-IVALS__)
}
