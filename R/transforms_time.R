# Time transformations

#' Compute elapsed time from beginning of POSIX vector
#'
#' @param x A vector of date/times POSIX objects
#' @param units The output units for the time difference
#'
#' @details The first member of x will have an elapsed time of 0.
#' Units can be specified as per \link[base]{difftime}.  Times are not sorted here,
#' so if they are not in order, or at least having the minimum value first, they should
#' be sorted beforehand.
#' @return The time difference for each member of x from the first member of x.
#' @export
#'
#' @examples
#' x = Sys.time() + c(0:10)*3600
#' as.numeric(elapsed.time(x))
#'
elapsed.time = function(x, units="hours")
{
  # x is a vector of POSIXct, with first time as the baseline for elapsed time
  difftime(x,x[1], units=units)
}

#' Fill NA values in a vector using previous non-NA value
#'
#' @param x A vector of values that may have NA values to fill
#' @param na A scalar or vector of values to treat as NA for purposes of filling
#' @param cond A logical scalar or vector.  NA will not be replaced if the corresponding condition is F.
#' @param backfill A logical indicating if initial NA values should be backfilled from the first non-NA value.
#'
#' @details This is an interpolation method called Last Observation Carried Forward.
#' It fills in a missing or NA value with the previous non-NA value.  If backfill=TRUE (default), any initial NA
#' values will be filled backward from the first non-NA value.  Multiple values can
#' be provided to consider as a match for NA.
#' @return The x vector with NA values replaced by previous non-NA value
#' @export
#'
#' @examples
#' x = c(1,"",2,3,"",".")
#' locf(x, na=c("","."))
#' locf(x, na=c("","."), cond=c(TRUE,TRUE,TRUE,TRUE,FALSE,TRUE))
#' locf(c(NA,NA,1,2,NA,NA,3))
#' locf(c(NA,NA,1,2,NA,NA,3), backfill=FALSE)

locf = function (x, na = NA, cond=T, backfill=T){
  #x is a vector.  na is the value to treat as na
  is.fac = inherits(x, "factor")
  if (is.fac)
    x = as.character(x)
  #if (!is.na(na)) removed this to allow for multiple na criteria. NA %in% c("",NA) evaluates T.
  x[x %in% na] = NA
  # Y is indicator, T if value is not missing
  Y = !(is.na(x) & (cond | is.na(cond)))
  if(backfill==T) x[] = c(x[Y][1], x[Y])[cumsum(Y) + 1]
  else x[] = c(NA, x[Y])[cumsum(Y) + 1]
  if (is.fac)
    x = as.factor(x)
  x
}

nocb = function(x, na=NA, cond=T, backfill=T){
  rev(locf(rev(x), na, rev(cond), backfill))
}

#' Create a character column based on named conditions
#'
#' @param ... Named arguments containing the conditions to evaluate
#' @param method If multiple conditions are true which should supercede, the first or last one encountered
#' @param default If no conditions are true, what value should be returned
#' @param asfactor If TRUE, return a factor with the default value as the first level
#'
#' @details Named arguments are evaluated inside the function to produce logical values.  The
#' returned value (which will be a vector if the conditions return vectors) contains the names of the
#' arguments that evaluate as TRUE.  All conditions should evaluate to the same length and each position
#' returns only one value.  The returned value is either the first or last matched condition (selected by
#' method) in the order provided, or the default value.
#' This function is particularly useful for evaluating criteria in a data.frame using plyr or dplyr
#' functions.
#' @return A vector of the names of the conditions that evaluated to TRUE, according to the method.
#' @export
#'
#' @examples
#' DV = c(1,2,3,NA,"BQL","No Sample",4)
#' conditional_values(BADDV=!is.number(DV), BQL=DV=="BQL", MISSING=is.na(DV), default="OK", method="last")

conditional_values = function(..., method=c("first", "last"), default="", asfactor=F){
  # ... are named conditions
  # method is first or last, if multiple conditions apply, which one supercedes?
  # default is the case where no rules apply
  # asfactor, T to return the column as a factor, F for string.  default will always be first factor, the rest in order

  method = match.arg(method)
  # this evaluates the rules into a named list of logical vectors
  conds = list(...)
  # organize into columns
  conds.m = do.call(cbind,conds)
  # check that the matrix is logical type
  if(!is.logical(conds.m)) stop("conditionalValues: conditions must evaluate to TRUE or FALSE")

  #let's always match first, so if method is last, reverse the order of columns
  if(method=="last") conds.m = conds.m[,rev(seq_len(ncol(conds.m)))]
  conds.m
  cnames=colnames(conds.m)
  if(length(cnames)!=ncol(conds.m)) stop("conditionalValues: Provide names for all conditions")

  # Then put the default in last column
  conds.m = cbind(conds.m, T)
  colnames(conds.m) = c(cnames,default)
  # T>F, so find first highest value, default comes last in search
  col.match = unlist(apply(conds.m, 1, which.max))
  values = colnames(conds.m)[col.match]
  if(asfactor) values = factor(values, levels=c(default,cnames))
  values
}

#' Determine if a value can be coerced to a numeric type
#'
#' @param x A value to test.  May be an vector.
#'
#' @return A vector of same size as x or T, if the corresponding value can be coerced, or F.
#' @export
#'
#' @examples
#' is.number(c(NA,"A","12"))

is.number = function(x){ suppressWarnings(!is.na(as.numeric(x)))}

#' Determine if a value can be coerced to an integer type
#'
#' @param x A value to test.  May be an vector.
#'
#' @return A vector of same size as x or T, if the corresponding value can be coerced, or F.
#' @export
#'
#' @examples
#' is.integral(c(NA,"A","12", "1.2"))

is.integral = function(x){
  y=suppressWarnings(as.numeric(x) %% 1==0)
  y[is.na(y)]=FALSE
  y
  }

#' Test if value in x is one of several values
#'
#' @details This is a functionalized form of x %in% c(...)
#' @param x A vector to test
#' @param ... quoted strings to test against
#'
#' @return A vector of logical (TRUE or FALSE) of length(x)
#' @export
#'
#' @examples
#' is_one_of(letters[1:5],"c","g")
is_one_of = function(x,...){
  x %in% c(...)
}

#' Convert an ISO8601 date/time to POSIXct
#'
#' @param dtc A ISO8601 formatted date/time
#' @param .time If dtc is date-only, a time to use as an offset.  If length is not 1 or
#' same as dtc, .time is recycled with a warning.
#' @param .tz The timezone for the dtc values.  Default is UTC.
#' @param .month If dtc is year only, a month to use a default.  If length is not 1 or
#' same as dtc, .time is recycled with a warning.
#' @param .day If dtc is year-month only, a month to use a default.  If length is not 1 or
#' same as dtc, .time is recycled with a warning.
#'
#' @details The conversion does its best to guess at the format of the time and to fill in any missing
#' parts, defaulting to Jan, the first of the month, and midnight.  The default day, month, and time can
#' be provided, but it's important to note that the infill of these values won't cross more than
#' one division.  That is, "1990" will default to "1990-01-01 00:00", even if .time is given as 06:35.
#' The missing pieces must be filled in in order.
#'
#' @return date/time as R POSIXct native type.
#' @importFrom anytime anytime
#' @export
#'
#' @examples
#' iso_to_posix("2016-10-03T21:22:45")
#' iso_to_posix(c("2016-10-03","2016-10-03T12:34"),"09:00")
#' iso_to_posix(c("2016-10-03","2016-10-03","2016-10-03"), c("09:00", "10:10")) #gives a warning about length of .times
#' iso_to_posix(c("1980", "1980-06", "1980-06-15"), .time="09:34")
#' iso_to_posix(c("1980", "1980-06", "1980-06-15"), .time="09:34", .day=15, .month=6)
iso_to_posix = function(dtc, .time, .tz="UTC", .day, .month){
  # check for year only and update with month
  if(!missing(.month)){
    if(!length(.month) %in% c(1,length(dtc))){
      # reuse .month by rep, and issue a warning
      warning(".month should be equal in length to dtc or of length 1.  .month has been replicated to the required length.")
    }
    if(length(.month)!=length(dtc)) .month=rep(.month, length.out=length(dtc))
    idx=grep("^[0-9]{4}$", dtc)
    dtc[idx]=sprintf("%s-%02d",dtc[idx], as.numeric(.month[idx]))
  }

  #check for yearn and month only in dtc, and if not present tack on day
  if(!missing(.day))
  {
    if(!length(.day) %in% c(1,length(dtc))){
      # reuse .day by rep, and issue a warning
      warning(".day should be equal in length to dtc or of length 1.  .day has been replicated to the required length.")
    }
    if(length(.day)!=length(dtc)) .day=rep(.day, length.out=length(dtc))
    idx=grep("^[0-9]{4}-[0-9]{2}$", dtc)
    dtc[idx]=sprintf("%s-%02d",dtc[idx], as.numeric(.day[idx]))
  }

  #check for T in dtc, and if not present tack on time
  if(!missing(.time))
  {
    if(!length(.time) %in% c(1,length(dtc))){
      # reuse .time by rep, and issue a warning
      warning(".time should be equal in length to dtc or of length 1.  .time has been replicated to the required length.")
    }
    if(length(.time)!=length(dtc)) .time=rep(.time, length.out=length(dtc))
    idx=grep("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", dtc)
    dtc[idx]=sprintf("%sT%s",dtc[idx], .time[idx])
  }

  #parsedate::parse_iso_8601(dtc) # replace parsedate with anytime here
  anytime(dtc, tz=.tz, asUTC=T)
}


#' Convert ISO duration to numeric
#'
#' @param x A character vector of ISO durations (e.g. PT23.5H)
#' @param units Units to output (defaults to hours)
#'
#' @return A numeric vector of the durations in x, in requested units
#' @export
#'
#' @examples
#' iso_duration(.(PT1H,-PT1H,PT.5H,-PT.5H,P1D,PT24H,P1W,PT336H))
#' iso_duration(.(PT1H,-PT1H,PT.5H,-PT.5H,P1D,PT24H,P1W,PT336H), "days")
iso_duration = function(x, units="h"){
  # case insensitive search if iso duration [-]P[*Y][*M][*W][*D][T][*H][*M][*S]
  s = "[0-9]*\\.?[0-9]*"
  regex_dur = sprintf("(?i)(-)?P(((%s)Y)?((%s)M)?((%s)W)?((%s)D)?)?(T((%s)H)?((%s)M)?((%s)S)?)?",s,s,s,s,s,s,s)
  # want columns 2(sign),5(Y),7(M),9(W),11(D),14(H),16(M),18(S)
  y=stringr::str_match(x,regex_dur)[,c(2,5,7,9,11,14,16,18), drop=F]
  # replace column two with 1 or -1
  y[,1]=ifelse(is.na(y[,1]),1,-1)
  # fill all other NA with zero
  y[is.na(y)]=0
  storage.mode(y)="numeric"
  # compute seconds (assume month is 30 days... should not be doing this with month)
  z = as.vector((y[,-1] %*% c(31536000,2592000,604800,86400,3600,60,1))*y[,1])
  # convert to specified units
  udunits2::ud.convert(z,"s",units)
}

#' Date and Time formatting for text output
#' @rdname datetimeformat
#' @details These functions simply provide a shorthand for the most common formats.
#'
#' @param x POSIXct values
#' @param format A format string. see \code{base::format}.
#' @param tz A time zone string, defaults to UTC (Univeral)
#'
#' @return The date or time formatted as text
#' @export
#'
#' @examples
#' format_date(Sys.time())
#' format_date(Sys.time(), format="%Y-%m-%d T %H:%M")
#' format_time(Sys.time())
format_date = function(x, format="%m/%d/%Y", tz="UTC"){
  format(x,format=format,tz=tz)
}


#' @rdname datetimeformat
#' @export

format_time = function(x, format="%H:%M:%S", tz="UTC"){
  format(x,format=format,tz=tz)
}


#' Replace values in a vector with supplied values
#'
#' @param x The vector in which to replace values
#' @param ... Name and value pairs, where name is what is replaced and value is what it is replaced with.
#' @param .not_found Value to use for unmatched values.  If not provided, unmatched values are unchanged.
#'
#' @return A vector with the replaced values.  An attempt is made to convert the return type to numeric, if possible.
#' @export
#'
#' @examples
#' replace_values(c("a","b","c"),a=1,b=2)
#' replace_values(c("a","b","c"),a=1,b=2, .not_found = NA)
#' replace_values(c("a","b","c"),a=1,b=2, .not_found = "")
#' replace_values(c("a","b","c"),a=1,b="hello", .not_found = "")
replace_values = function(x, ..., .not_found=NULL)
{
  # get unique values to look for
  dots=list(...)
  uniques=names(dots)
  # get values to replace
  values=unlist(dots)
  # get indices into uniques, 0 for no match
  idx=match(x,uniques,0)
  # replace matches
  x[as.logical(idx)]=values[idx]
  # replace non-matches
  if(!is.null(.not_found)) x[idx==0]=.not_found
  type.convert(as.character(x),as.is=T)
}

#' Time difference
#'
#' @param x1 A Posixct date/time
#' @param x2 A Posixct date/time
#' @param units A string giving units of: secs, min, hours, days, weeks, months, years
#'
#' @return a pmunits vector with the time difference expressed in the chosen units
#' @export
#'
#' @examples
#' time_diff(iso_to_posix("2001-10-01T12:00"),iso_to_posix("2000-10-01T12:00"), units="years")
#' time_diff(iso_to_posix("2001-10-03T12:00"),iso_to_posix("2001-10-01T12:00"), units="days")
#' time_diff(c(iso_to_posix("2001-10-03T12:00"),iso_to_posix("2001-10-02T12:00")),iso_to_posix("2001-10-01T12:00"), units="hours")
time_diff = function(x1, x2, units="hours"){
  convert(set_units(as.numeric(difftime(x1,x2,tz="UTC",units="secs")), "s"),.from="s", .to=units)
}

#' Increments a counter when the criteria is met
#'
#' @param criteria A logical expression that is TRUE when the count should increment
#' @param fill.na TRUE if NA values in criteria should be filled with FALSE or FALSE to leave them as NA.
#'
#' @return A vector with the running count of times the criteria has been met.
#' @export
#'
#' @examples
#' EVID=c(1,1,0,0,1,1,1,0)
#' CMT= c(1,2,1,2,1,2,NA,2)
#' count_if(EVID==1 & CMT==1)
#' count_if(EVID==1 & CMT==1, fill.na=FALSE)
count_if = function(criteria, fill.na=T)
{
  x=criteria
  if(fill.na) x[is.na(x)]=FALSE
  cumsum(x)
}

#' Enumerate on condition 1 when followed by condition 2
#'
#' @param x1 The main condition on which to enumerate
#' @param x2 A predicate condition which must be TRUE following x1
#' as TRUE, in order to increment the occasion.
#' @param .lead_in One of "combine" (default), "separate" or "missing" to indicate what
#' to do for elements appearing before the first occasion.
#'
#' @return An integer array, of the length of x1, enumerating occasions.
#' @details x1 and x2 are expressions that evaluate to TRUE or FALSE.
#' When x1 is TRUE a counter is incremented, but only if x2 is TRUE
#' prior to the next value of x1 being TRUE.  That is, consecutive x1 TRUE
#' values count as a single occasion.  The occasion is only incremented
#' if an x2 value of TRUE falls between TRUE values of x1.  Note that x2
#' can fall anywhere between TRUE values of x1 - the exact position is
#' not specified.
#' @importFrom dplyr data_frame mutate filter if_else
#' @export
#'
#' @examples
#' library(dplyr)
#' # increment occasion only when 1 is followed by 2.
#' vals = c(1,2,3,2,3,2,1,1,1,2,3,2,1,2,1,1,2,2,2,1,2,1,2,1,2)
#' occasion(vals==1, vals==2)
#' #' # increment occasion only when 1 is followed by 2, .lead_in options.
#' vals = c(2,1,1,2,3,2,3,2,1,1,1,2,3,2,1,2,1,1,2,2,2,1,2,1,2,1,2)
#' occasion(vals==1, vals==2, .lead_in="combine")
#' occasion(vals==1, vals==2, .lead_in="separate")
#' occasion(vals==1, vals==2, .lead_in="missing")
#' # increment occasion only when 1 is IMMEDIATELY followed by 2.
#' vals = c(1,2,3,2,3,2,1,1,1,2,3,2,1,2,1,1,2,2,2,1,2,1,2,1,2)
#' occasion(vals==1 & lead(vals)==2)
#' # increment when evid=0 and cmt 2 is observed
#' evid = c(1,0,0,0,0,0,1,0,0,1,0,1,1,0,1,0,0,0,0,1,0,1,0,1,0)
#' occasion(evid==1, evid==0 & vals==2)
occasion = function(x1, x2=TRUE, .lead_in=c("combine", "separate", "missing")){
  .lead_in=match.arg(.lead_in)
  df = data_frame(X1=x1==TRUE, X2=x2==TRUE) %>%
    mutate(rowid=1:dplyr::n()) %>%
    filter(X1==TRUE | X2==TRUE) %>%
    mutate(start_occ = if_else(X1==TRUE & lead(X2)==TRUE, TRUE, FALSE)) %>%
    filter(start_occ==TRUE)
  occ = rep(F, length(x1))
  occ[df$rowid]=TRUE
  # cumulative sum of changepoint rows
  occ = cumsum(occ)
  # handle lead in occasion
  if(.lead_in=="missing"){
    occ[occ==0] = NA
  } else if(.lead_in=="combine"){
    occ[occ==0] = 1
  } else if(.lead_in=="separate"){
    occ = occ+1
  }
  occ
}
