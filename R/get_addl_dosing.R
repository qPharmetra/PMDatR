seconds_into_day = function(x){as.integer(x) %% 86400}
roundup = function(x) floor(x+.5)
#' derive necessary information about addl and append to given dataframe
#' @param df dataframe
#' @param .f function to calculate how to calculate addl for non-nominal times
#' @param .tolII A number between 0 and 1 that determines the fraction
#' of the dosing interval that must pass before an ADDL sequence can begin.
#' @export
get_addl_dosing <- function(df, .f = roundup, .tolII=0.5) {
  # expectations for function
  # MEX/MEX__ column containing whether missing DOSE, is 1/0
   mex_exists <- FALSE
  if ("MEX" %in% names(df)) {
    mex_exists <- TRUE
    if (is.character(df$MEX) || is.factor(df$MEX)) {
      df <- df %>% mutate(MEX__ = as.numeric(as.factor(MEX)) - 1)
    } else {
      # already numeric or boolean, hopefully
      df <- df %>% mutate(MEX__ = MEX)
    }
  } else {
    # if no MEX, assume all doses are legit
    df <- df %>% mutate(MEX__ = 0)
  }
  # use a dose indicator to determine what amounts for imputed addl values should
  # actually be, given originally a missing dose value
  if ("DOSE" %in% names(df)) {
    df <- df %>% mutate(DOSE__ = DOSE)
  } else {
    # algorithm will be, to collect all non-zero AMT records, then fill
    # based on other doses with the same II within that group
    original_groups <- as.character(dplyr::groups(df))

    df <- df %>% mutate(DOSE__ = ifelse(AMT == 0, NA, AMT)) %>%
      dplyr::group_by_(c(original_groups, "II")) %>%
      tidyr::fill(DOSE__, .direction = "down") %>%
      tidyr::fill(DOSE__, .direction = "up")

    if (!length(original_groups)) {
      df <- dplyr::ungroup(df)
    } else {
      df <- dplyr::group_by_(df, .dots=original_groups)
    }
  }
  df_times <- df %>%
    dplyr::mutate(
      # get the time in seconds into the day for the next valid dose
      # to be used to increment the time of the missing dose record to match proposed spec
      # assumes MEX will be a flag of 0/1
      SECONDS_IN_DAY__ = seconds_into_day(TIME),
      # next actual dose seconds in day
      NDTIME_SECONDS__ = ifelse(MEX__, NA, SECONDS_IN_DAY__)
    ) %>%
    #this should allow also grouping by sequences
    # so if an ID has multiple regimens, will get the
    # next dose time from a dose in the appropriate sequence
    tidyr::fill(NDTIME_SECONDS__, .direction = "up")
  # ungroup now that have the next dose time of day for any in same period
  df_times <- df_times %>%
    dplyr::mutate(
      # this will coerce any posixct time to seconds since epoch, so from here out we will work in
      # terms of seconds until a final conversion back
      NDSECONDS_IN_DAY__ = dplyr::lead(NDTIME_SECONDS__, 1),
      TIME = ifelse(
        MEX__,
        # for BID/TID dosing, the time should also be incremented/decremented to align
        # with the next dose in intervals - eg given a next observed BID dose at 8 am
        # the TIME should be adjusted to 8 pm
        TIME + (
          guess_dosing_sequence(SECONDS_IN_DAY__ / 3600, II) -
            guess_dosing_sequence(NDSECONDS_IN_DAY__ / 3600, II)
        ) * II * 3600 +
          NDSECONDS_IN_DAY__ - SECONDS_IN_DAY__,
        TIME
      ),
      # get D(ifference) in time between TIME and N(ext)TIME
      NTIME__ = dplyr::lead(TIME, 1),
      DTIME__ = NTIME__ - TIME
    )
  addl_times <- df_times %>%
    # get dosing records that are non-consecutive
    # using a buffer of tolII as may have a slightly longer next time (eg for BID 12.2 hrs)
    dplyr::filter(II > 0, DTIME__ > II * (.tolII) * 3600)
  if(nrow(addl_times)){
    addl_times = addl_times %>%
    # increment time and DTIME to correspond to a dose starting the next dosing interval
    dplyr::mutate(
      DTIME__ = DTIME__ - II * 3600,
      ADDL = calc_addl(DTIME__, II * 3600, .f = .f, .tol=.tolII),
      # MEX__ records already normalized to next proper dose record,
      # this will also take all dose records and adjust their time to be
      # the aligned with the time of the next dose,
      TIME = ifelse(
        !MEX__,
        NTIME__ - (ADDL+1)*II*3600,
#         TIME + (
#           guess_dosing_sequence(SECONDS_IN_DAY__ / 3600, II) -
#             guess_dosing_sequence(NDSECONDS_IN_DAY__ /
#                                     3600, II)
#         ) * II * 3600 +
#           NDSECONDS_IN_DAY__ - SECONDS_IN_DAY__,
        TIME + II * 3600
      ),
      TIME = anytime::anytime(TIME, tz = "UTC", asUTC = TRUE),
#       DTIME__ = DTIME__ - II * 3600,
#       ADDL = calc_addl(DTIME__, II * 3600, .f = .f),
      EVID = 1,
      # addl dose should always be as if from proper dosing record so set these to as if exist
      MEX = FALSE,
      MEX__ = 0
    ) %>%
    filter(ADDL>=0) # remove negative addl computed for fractional interval
  } else {
    # add the ADDL column as empty
    addl_times$ADDL = numeric(0)
  }
  output_dosing <- dplyr::bind_rows(df_times, addl_times) %>%
    # when binding time can get kicked back to doubles again so back to UTC for a final time
    dplyr::mutate(TIME = anytime::anytime(TIME, "UTC", TRUE)) %>%
    dplyr::arrange(TIME) %>%
    dplyr::mutate(# dose imputation will need to be further discussed - can take next dose, prior dose etc
      AMT = ifelse(AMT == 0 & MEX__ == F & EVID==1, DOSE__ , AMT)) %>%
    # final cleanup
    dplyr::select(
      -DTIME__,
      -NTIME__,
      -SECONDS_IN_DAY__,
      -NDTIME_SECONDS__,
      -NDSECONDS_IN_DAY__,
      -MEX__
    ) %>%

    dplyr::select(-DOSE__) %>%
    tidyr::replace_na(list(ADDL = 0, II = 0)) %>%
    # for II any records that have an II but no addl dosing should be 0
    mutate(II = ifelse(ADDL == 0, 0, II))

  if (!length(original_groups)) {
    output_dosing <- dplyr::ungroup(output_dosing)
  } else {
    output_dosing <- dplyr::group_by_(output_dosing, original_groups)
  }
  if (!mex_exists){
    output_dosing <- dplyr::select(output_dosing, -MEX)
  }
  return(output_dosing)
}
