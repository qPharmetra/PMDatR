#' visualize dosing history
#' @param .df dosing dataframe
#' @param .size size of points
#' @details
#' need columns TIME
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_discrete theme_bw labs
#' @export
visualize_dosing <- function(.df, .size = 3) {
  .df %>%
  ggplot(aes(x = TIME, y = factor(AMT))) +
    geom_point(aes(shape = factor(ifelse(ADDL == -1, 2,
                                     ifelse(ADDL ==0, 1, 3)))), size = .size) +
    scale_shape_discrete(name = "Record Type",
                         labels = c("Original", "Expanded", "Derived")) +
    theme_bw() +
    labs(x = "Time", y = "Dosing Amount")
}

#' overlay dosing records onto an existing ggplot
#' @param .p ggplot object
#' @param .df dosing dataframe including TIME and ADDL columns
#' @importFrom ggplot2 scale_linetype_discrete geom_vline
#' @export
overlay_dosing <- function(.p, .df) {
  .p + geom_vline(data = .df,
                  # just time will choke as xintercept cannot accept POSIXt values
                  # so coercing to numeric, will not affect plots, they will still show
                  # as datetime axis labels
             aes(xintercept = as.numeric(TIME),
                 linetype = factor(ifelse(ADDL == -1, 2,
                               ifelse(ADDL ==0, 4, 3)))
                             )) +
    scale_linetype_discrete(
      name = "Record Type",
      labels = c("Original", "Derived", "Expanded")
    )
}
