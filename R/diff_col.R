# calculate the difference based on some criteria
# @param .df
# @param .col
# @param .grps
# @param ... filter criteria to determine baseline, will take first non-na value in grp
# @param .name_fn function to name columns, passes the name of the column to function
# @examples \dontrun {
# diff_col(Theoph, "conc", "Subject", between(Time, 4, 8) | (Subject == 3 & Time > 0), .name = "other")
#}
# @details
# diff_col gets the first value (per group) given some filter criteria, and calculates
# the difference from the original value.
#
# This provides the fundametal building block for most of the change from x calculations
# @return list with output df, .col = name of column, .diff_name is the name of the diff col

diff_col <- function(
           .df,
           .col,
           .grps = NULL,
           ...,
           .name = NULL,
           .name_fn = NULL,
           .agg_fn = NULL) {
    .df <- dplyr::ungroup(.df) %>% mutate(INDEX__ = 1:dplyr::n())
    if (is.null(.name)) {
      if (!is.null(.name_fn)) {
        .name <- .name_fn(.col)
      } else {
        .name <- paste0("DIFF_", .col)
      }
    }
    #dots <- lazyeval::dots_capture(...)
    dots <- lazyeval::lazy_dots(..., .follow_symbols = T)
    if (length(dots)) {
      if (!is.null(.grps)) {
        bl_df <- .df %>%
          group_by_(.dots = .grps) %>%
          filter_(.dots = dots)
      } else {
        bl_df <- .df %>% filter_(.dots = dots)
      }
    } else {
      bl_df <-
        .df # copy whole df, will eventually slice the first value per group to establish baseline
    }
    if (!is.null(.grps)) {
      bl_df <- bl_df %>%
        group_by_(.dots = .grps) %>%
        filter_(.dots = lazyeval::interp(~!is.na(.col), .col = as.name(.col)))
      # want to note which indices are related to BL values
      # if .agg_fn is present aggregate calculation rather than slicing
      if (!is.null(.agg_fn)) {
      .df <- .df %>%
        left_join(bl_df %>% ungroup %>% select(INDEX__) %>% mutate(IS_DIFF__ = 1)) %>%
        mutate(IS_DIFF__ = ifelse(is.na(IS_DIFF__), 0, IS_DIFF__))
       bl_df <- bl_df %>%
         dplyr::summarize_(.dots = setNames(list(
           lazyeval::interp( ~ .agg_fn(.col),
                             .col = as.name(.col))
         ), .col))
      } else {
       bl_df <- bl_df %>%
        dplyr::slice(1L)
      .df <- .df %>% left_join(bl_df %>% ungroup %>% select(INDEX__) %>% mutate(IS_DIFF__ = 1)) %>%
        mutate(IS_DIFF__ = ifelse(is.na(IS_DIFF__), 0, IS_DIFF__))
      }
        # need to ungroup before selecting, or otherwise groups that no
        # longer have a value will cause an error

      bl_df <- bl_df %>% ungroup %>%
      select_(.dots = c(unlist(.grps), .col)) %>%
      dplyr::rename_("DIFF__" = .col)

      # set indices associated with the diffs
      combined_df <- left_join(.df, bl_df)
    } else {
      # ungrouped, just take the first row and use the value in the col
      # for the DIFF__ col
      if (!is.null(.agg_fn)) {
       bl_value <- bl_df %>%
         summarise_(.dots = setNames(list(
           lazyeval::interp( ~ .agg_fn(.col),
                             .col = as.name(.col))
         ), .col))
      } else {
        bl_value <- bl_df %>%
          dplyr::slice(1L) %>% select_(.col)
      }

      combined_df <- .df %>% mutate(DIFF__ = bl_value[[.col]])
    }
    output <- combined_df %>%
      mutate_(.dots = setNames(list(
        lazyeval::interp( ~ .col - DIFF__,
                          .col = as.name(.col))
      ), .name))

    if (length(dots) && !is.null(.agg_fn)) {
      output[[.name]] <- ifelse(output$IS_DIFF__ == 1, 0, output[[.name]])
    }

    return(list(
      df = output %>% select(-INDEX__, -IS_DIFF__),
      .col = .col,
      .diff_name = .name
    ))
  }

