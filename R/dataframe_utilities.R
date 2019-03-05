#' Combine data frame rows by tolerance on a value
#'
#' @param df A dataframe object
#' @param values the quoted name of the column on which to combine rows
#' @param tol The tolerance within value in which to combine rows
#' @param ... Summary function to adjust or create columns within the combined row
#' @return the modified dataframe
#' @importFrom lazyeval expr_find lazy_dots
#' @importFrom dplyr groups mutate_ group_by_ ungroup slice select_
#' @export
#' @examples
#' set.seed(12345)
#' vals = runif(100,0,100)
#' vals = vals[order(vals)]
#' data1 = data.frame(TIME=vals, SEX=sample(x=0:1,size = 100,replace=TRUE), AMT=100)
#' # make groups where start is within tol=0.5
#' combine_rows_by_tol(data1, values="TIME", tol=0.5, AMT=sum(AMT))
combine_rows_by_tol = function(df, values="TIME", tol=0, ...){
  # df is a data frame
  if(!inherits(df,"data.frame")){stop("argument [df] must be a dataframe.")}
  # values is the column to combine on
  values=as.character(expr_find(values))
  if(!inherits(df[[values]], c("numeric","POSIXct"))){
    stop("argument [values] must be a numeric or time (POSIXct) type.")
  }
  # tol is the tolerance to combine values
  if(!inherits(tol ,c("numeric","POSIXct"))){
    stop("argument [tol] must be a numeric or time (POSIXct) type.")
  } else {
    if(tol<0){stop("argument [tol] must be a positive numeric or time (POSIXct) type.")}
  }

  # this bit may be slow  we can look to speed it up in it's own function, perhaps
  grp=integer(nrow(df))
  vals=as.numeric(df[[values]]) # don't index data.frame inside loop
  xnext=vals[1]+tol
  val.na = is.na(vals)
  gnum=1

  for(i in 1:length(grp)){
    if(!val.na[i]){
      if(vals[i]>xnext){
        gnum=gnum+1
        xnext=vals[i]+tol
      }
      grp[i]=gnum
    } else {
      grp[i]=-i # if NA assign unique groupid to avoid combining NA values
    }
  }

  dfnames=names(df) # keep existing names only
  dfgroups = groups(df) # to reset existing groups
  df[["n__"]]=1:nrow(df) # to restore sorting
  df[["groupid__"]]=grp # assign group id variable
  newdoses = df %>%
    group_by_(.dots=c(dfgroups, "groupid__")) %>%
    # run any ... expressions to summarize on the groups (e.g. AMT=sum(AMT))
    mutate_(.dots=lazy_dots(...)) %>%
    # take just the first record
    slice(1) %>%
    # ungroup so we can drop all groupid__
    ungroup() %>%
    # reset original order
    arrange(n__) %>%
    # keep only the original columns
    select_(.dots=dfnames) %>%
    # restore original groups
    group_by_(.dots=dfgroups)

  return(newdoses)
}



#' Split rows based on a delimited string column
#'
#' @param df A dataframe
#' @param col The column to split on
#' @param sep The separator characters
#'
#' @return A dataframe
#'
#' @details This function splits a dataframe's rows based on delimited values in a column.  For instance,
#' a row named "Children" with "Tommy, Gillian, Betsy" would be duplicated three times with the children
#' listed separately on each row.
#' @importFrom dplyr mutate_ group_by_ groups
#' @importFrom tidyr unnest_
#' @importFrom lazyeval interp
#' @export
#'
#' @examples
#' library(dplyr)
#' data_frame(Parent=c("Homer, Marge", "Lorelai", "Peter, Lois"), Child=c("Bart, Lisa, Maggie",
#' "Rory", "Chris, Meg, Stewey")) %>% separate_rows("Parent") %>% separate_rows("Child")
separate_rows = function(df, col, sep=", "){
  grps = groups(df)
  mutate_(df, .dots = setNames(list(interp(~strsplit(as.character(x), y),
                                                            x=as.name(col), y=sep)),col)) %>%
    ungroup() %>%
    unnest_(col) %>%
    group_by_(.dots=grps)
}



## Start with CM or AE domain with a column of CODES and START and STOP times
## do any prior data cleaning, such as removing duplicates, unwanted records

#' Convert a dataframe of codes with start/stop time to a flat, time-varying covariate format
#'
#' @param df A dataframe (e.g. CM or AE domain)
#' @param codes The name of a column that holds codes that will become columns
#' @param stdtc An expression for the start time (e.g. CMSTDTC)
#' @param endtc An expression for the end time (e.g. iso_to_posix(CMSTDTC, .time="24:00"))
#' @param start_val An expression for the value to assign at the start time
#' @param end_val An expression for the value to assign at the end time
#' @param TIME A name for the new time column
#' @param .sticky A value (set by start_val or end_val) which should be set permanently
#'
#' @details The function does the heavy lifting of separating codes into columns that are
#' flagged at their start and stop times.  The names of the columns are the codes provided in
#' df.  There is some setup that may be necessary to make proper use of this function:
#' \itemize{
#'    \item Set grouping before calling.  The groups will be preserved.
#'    \item The times are assumed to be POSIXct or numeric.  Conversions can be done using the
#'    mappings stdtc and endtc.
#'    \item Clean up the data to remove duplicate rows and create short codes that will make
#'    suitable column names.
#'    \item filter out codes that are not of interest.  The function will create columns for
#'    ALL of the codes.
#'    \item the output of this function is inteded to be used in a Time-Varying Covariate query.
#'    Records occurring beyond the limits of the study can be removed there.  The output keeps
#'    the original data on the rows to aid in further pruning/mapping of the dataset.
#'    \item The start_val and end_val mappings can be complex expressions to take into account
#'    not only the codes, but other columns as well.  The values can be set arbitrarily.
#' }
#'
#' @return A reformatted dataframe
#' @importFrom lazyeval expr_find lazy
#' @importFrom dplyr mutate_ filter mutate arrange bind_rows rename_ group_by_ mutate_at
#' @importFrom tidyr fill_ replace_na
#' @export
#'
#' @examples
#' \dontrun{
#' cm %>% filter(CMDECOD %in% .(ATORVASTATIN,PARACETAMOL,IBUPROFEN,METFORMIN)) %>%
#'   group_by(USUBJID) %>%
#'   time_varying_codes(CMDECOD,
#'     iso_to_posix(CMSTDTC,.time = "00:00"),
#'     iso_to_posix(CMENDTC,.time = "24:00"),
#'     ifelse(CMENDTC=="",2,1),
#'     .sticky = 2) -> cm.out
#' }
time_varying_codes = function(df, codes, stdtc, endtc, start_val=1, end_val=0, TIME="TIME", .sticky=NULL){
  # codes is the column with codes to use to create variables
  # stdtc and endtc are expressions to compute the starting and ending times.  consider setting missing
  #  times to 00:00 when starting, and 24:00 when ending, which will span the entire day

  codes = as.character(expr_find(codes))
  grps = groups(df)
  dfstart = df %>% mutate_(.dots = setNames(list(lazy(stdtc, .follow_symbols=TRUE),
                                                 lazy(start_val, .follow_symbols=TRUE)),
                                            c("TIME__","VALUE__"))) %>%
    mutate(ORDER__=0)

  dfend = df %>% mutate_(.dots = setNames(list(lazy(endtc, .follow_symbols=TRUE),
                                               lazy(end_val, .follow_symbols=TRUE)),
                                          c("TIME__","VALUE__"))) %>%
    filter(!is.na(TIME__)) %>%
    mutate(ORDER__=1)
  # the ordering variable allows us to sort so if we have alternating starting and ending times such that there
  # are duplicate times
  out= bind_rows(dfstart, dfend) %>% arrange(TIME__, ORDER__)


  # allow for nested sequencing of codes (on, on, off, off) by computing cumulative codes
  out = out %>% group_by_(.dots=c(grps,codes)) %>%
    mutate(accum__=cumsum((ORDER__==0) - (ORDER__==1)),
           dropme__ = (ORDER__==1 & accum__>0) | (ORDER__==0 & accum__>1) &
                                                    if(is.null(.sticky)) TRUE else VALUE__!=.sticky) %>%
    # drop rows with ORDER__==1 and accum__>0 (not a real endpoint)
    # drop rows with ORDER__==0 and accum__>1, unless VALUE__==.sticky (it's a duplicate start)
    filter(!dropme__) %>%
    ungroup()

  #sanitize newcols so they are safe to use as column names
  code.fac = factor(out[[codes]])
  levels(code.fac) = make.names(levels(code.fac))
  out[[codes]]=as.character(code.fac)
  newcols = unique(out[[codes]])

  out = out %>% tidyr::spread_(key=codes, value = "VALUE__") %>%
    group_by_(.dots=grps) %>%
    arrange(TIME__, ORDER__) %>%
    fill_(fill_cols=newcols, .direction="down") %>%
    replace_na(setNames(as.list(rep(0,length(newcols))), newcols)) %>%
    rename_(.dots=setNames(list("TIME__"),TIME)) %>%
    select(-ORDER__, -accum__, -dropme__)

  sticky_fill = function(x, .sticky){
    if(is.null(.sticky)) {return(x)}
    # find first value of .sticky
    idx = match(.sticky,x)
    if(!is.na(idx)){
      # rep x before 2 and fill 1 after
      x=c(x[1:idx-1], rep(.sticky,length(x)-idx+1))
    }
    x
  }
  if(!is.null(.sticky)){
    out = out %>% group_by_(.dots=grps) %>%
      mutate_at(newcols, funs(sticky_fill), .sticky=.sticky) %>%
      ungroup()
  }
  out %>% group_by_(.dots=grps) %>% arrange(TIME)
}


#' Join two dataframes with partial matching of keys
#'
#' @param x A dataframe
#' @param y A dataframe
#' @param by_x Column to join in x
#' @param pattern_y Column to join in y, containing patterns which may result in multiple matches
#'
#' @return A dataframe
#' @details If by_x has multiple values that match rows of pattern_y, multiple rows are created
#' by the join.
#' @importFrom  dplyr bind_cols
#' @export
#'
#' @examples
#' library(dplyr)
#' x=data_frame(A=c("AA BB","CC","DD EE"), B=1:3)
#' y=data_frame(K=c("AA","BB","CC","DD"), Z=c("cat1","cat1","cat1","cat2"))
#'partial_join(x,y,"A","K")
partial_join <- function(x, y, by_x, pattern_y){
  idx_x <- sapply(y[[pattern_y]], grep, x[[by_x]])
  idx_y <- sapply(seq_along(idx_x), function(i) rep(i, length(idx_x[[i]])))

  df <- bind_cols(x[unlist(idx_x), , drop = F],
                         y[unlist(idx_y), , drop = F])
  return(df)
}
