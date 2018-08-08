## PMDatR functions for merging datasets in the merge process

#' Append event data frames
#'
#' @param ... Data frames to append
#'
#' @return A tbl_df object with the appended data
#'
#' @details If there are mismatched column names the resulting column will have
#' NA entries for the records sourced from the input data frame that lacks those columns.
#' For instance, if A.df and B.df input: \code{append.events(A.df, B.df)}, with A.df having
#' columns {ID, TIME, x3} and B.df having columns {ID, TIME, x4}, the resulting dataframe
#' will have columsn {ID, TIME, x3, x4}.  Some of the entries for x3 and x4 will be NA.
#'
#' NA values in columns should be addressed after the merge process is completed.
#'
#' All input data frames should have ID and TIME columns.
#'
#' @importFrom dplyr arrange bind_rows "%>%"
#' @importFrom lazyeval lazy_dots
#' @export
#'
append.events = function(...){
  # check for NULL arguments.  NULLs can be passed through this without failure, but we want a warning.
  null_args = sapply(lazy_dots(...), function(x) is.null(x$expr))
  if(any(null_args)) warning(sprintf("Warning: Argument %s is NULL.  Be sure that this is intended.", null_args))

  bind_rows(..., .id=NULL) %>% arrange(ID,TIME)
}

#' Append time-varying covariates to an event data frame
#'
#' @param event.df The event dataframe to append to
#' @param ... Time-varying covariate dataframes to append
#'
#' @return A tbl_df object with the appended data
#'
#' @details If there are mismatched column names the resulting column will have
#' NA entries for the records sourced from the input data frame that lacks those columns.
#' For instance, if A.df and B.df input: \code{append.events(A.df, B.df)}, with A.df having
#' columns {ID, TIME, x3} and B.df having columns {ID, TIME, x4}, the resulting dataframe
#' will have columsn {ID, TIME, x3, x4}.  Some of the entries for x3 and x4 will be NA.
#'
#' This is expected behavior for merging time-varying covariates, because the covariates
#' are not likely to have been measured at the same time as dosing and obersations.
#' Specifically, dose and observation records will likely have NA values for the covariate
#' columns.
#'
#' NA values in columns should be addressed after the merge process is completed.
#'
#' All input data frames must have ID and TIME columns.
#'
#' @importFrom dplyr arrange bind_rows
#' @importFrom lazyeval lazy_dots
#' @export
#'
append.CovT = function(event.df, ...){
  # check for NULL arguments.  NULLs can be passed through this without failure, but we want a warning.
  null_args = sapply(lazy_dots(...), function(x) is.null(x$expr))
  if(any(null_args)) warning(sprintf("Warning: Argument %s is NULL.  Be sure that this is intended.", null_args))

  bind_rows(event.df, ..., .id=NULL) %>% arrange(ID,TIME)
}

#' Merge events with covariates
#'
#' @param events.df The events dataframe into which to merge the covariates
#' @param covs.l A list of covariate tbl_df dataframes to merge into events.df
#'
#' @return A tbl_df object with the merged data
#'
#' @details The events dataframe is merged with each tbl_df according to the
#' keys for the covariates.  Each member of covs.l may have its own keys, which
#' are stored in the groups property for the object.  Hence, merging can be controlled
#' by changing the grouping of the covariate dataframes using dplyr::group_by.
#'
#' By default a left join is performed, so all records in events.df are preserved.  Unmatched key
#' values in the covariate data will result in NA in the covariate column.  Covariate data
#' with unmatched keys will be silently dropped.  For example, if merging covariates by
#' ID and VISIT, if the SCREENING visit was not included in the covariate data then
#' observations or doses occuring in (or associated with) the SCREENING VISIT will have
#' NA values for the covariates.  Conversely, if the Follow-up VISIT is not present in the event
#' data, no covariate data is present in the result for the Follow-up visit.  This ensures that
#' new rows are not added that would not have the correct variables (TIME, AMT, DV, EVID, etc.)
#' set.
#'
#' To use other types of joins, set a join_type attribute on the data_frame objects in covs.l.
#' The available options are: left, right, inner, full.  Any other value will default to a left join.
#'
#' @importFrom dplyr groups left_join right_join inner_join full_join "%>%"
#' @export merge.Cov
#'
merge.Cov = function(events.df, covs.l){
  # check events.df NULL or missing
  if(missing(events.df)) {stop("Error: events.df is missing.")}
  if(is.null(events.df)) {stop("Error: events.df is NULL.")}
  if(missing(covs.l)) {stop("Error: covs.l is missing.")}
  if(is.null(covs.l)) {warning("Warning: covs.l is NULL.  Be sure this is intended.")}

  for(cov.df in covs.l){
    #get the keys from the cov.df
    keys = as.character(groups(cov.df))
    #get the join type and switch the function out
    jtype = attributes(cov.df)$join_type
    if(is.null(jtype)) {jtype = "left"}
    # check for duplicate column names in each data frame
    dupcols = intersect(names(events.df),names(cov.df))
    dupcols = setdiff(dupcols, keys)
    if(length(dupcols)>0){
      msg = paste0("Columns [%s] appear in both events and covariates and are not keys. ",
                   "These columns will be duplicated and renamed in the output, which ",
                   "will cause problems in continuing with the data merge. Please ensure ",
                   "the only covariate column names that match observation or dosing ",
                   "column names are used as merge keys.")
      warning(sprintf(msg,paste(dupcols,collapse=", ")))
    }
    fun = switch(jtype,
                 right = right_join,
                 inner = inner_join,
                 full = full_join,
                 left = , #left and anything else fall through to left_join
                 left_join)

    events.df = events.df %>% fun(cov.df, by=keys)
  }
  events.df
}


#' Perform routine generic checks on NONMEM data and apply post-merge functions
#'
#' @param .data The NONMEM database file
#' @param fun.transform A function that applies transformations
#' @param fun.filter A function that applies filters
#' @param fun.exclude A functiona that applies exclusion rules
#'
#' @return The refactored .data object
#' @importFrom dplyr "%>%" mutate group_by ungroup row_number
#' @export
#'

post.merge.refactoring = function(.data, fun.transform, fun.filter, fun.exclude, options=NULL){

  # set default options to ensure all defaults needed are covered
  def_options = list(
    KeepEvid2 = F,
    EVIDorder = c(2,1,3,4,0),
    SortOrder = c("ID","TIME","EVID",intersect(names(.data),"CMT")), #CMT only if in the dataset
    DropEmptyColumns=T
  )
  if(is.null(options)){ options=def_options}
  else {

    options = modifyList(def_options,options)
    }

  # to be called after all merging is complete
  # transform, filter and exclude are supplied functions
  cols = names(.data)
  col.typ = sapply(.data, class)
  # first check the data for bad values.
  # expect ID, DV, EVID
  if(!all(c("ID","DV","EVID") %in% cols)) stop("Post Merge: NONMEM dataset must have ID, DV, and EVID columns")

  # suspect TIME, AMT, CMT
  if(!("TIME" %in% cols)) warning("Post Merge: NONMEM dataset does not have TIME column")
  if(!("AMT" %in% cols)) warning("Post Merge: NONMEM dataset does not have AMT column")
  if(!("CMT" %in% cols)) warning("Post Merge: NONMEM dataset does not have CMT column")

  # check types
  if(!is.numeric(.data[["DV"]])) stop("Post Merge: Non-numeric value detected in DV column")
  if(!is.numeric(.data[["EVID"]])) stop("Post Merge: Non-numeric value detected in EVID column")

  # TIME must be posixct or numeric (no NA)
  if(!("POSIXct" %in% col.typ[["TIME"]])) stop("Post Merge: TIME column must be class POSIXct.
                                             Try using PMDatR transformation functions to create the TIME variable")
  # columns where NA not allowed
  .data = .data %>% fill_NA(AMT, 0)

  tryCatch({
    # Do sorting before any additional transformation.  Default sorting is on ID, TIME, EVID, CMT (if present)
    ## EVID sort order 2,1,3,4,0 -> generate ordered factor __EVID_SORT then sort and drop
    .sortcols = options$SortOrder[gsub("^-", "", options$SortOrder) %in% names(.data)]
    .sortused = .sortcols
    .sortdropped = setdiff(options$SortOrder, .sortused)
    # replace EVID by EVID_SORT__
    .sortcols = gsub("^(-)?EVID$","EVID_SORT__",.sortcols)
    # replace "-x" with "desc(x)"
    .sortcols = gsub("^(-)(.*)$", "desc(\\2)",.sortcols)
    .data = .data %>% ungroup %>% mutate(EVID_SORT__ = factor(EVID,levels=options$EVIDorder)) %>%
      arrange_(.dots=.sortcols) %>% select(-EVID_SORT__)},
    error = function(cond){
      stop("Post Merge: Unable to process Sorting.  Check sort column and EVID ordering options")},
    finally={
      message(sprintf("Post Merge sort order [%s] and EVID order [%s]",
                      paste(.sortused, collapse=", "),
                      paste(options$EVIDorder, collapse=", ")))
      if(length(.sortdropped))
        message(sprintf("Columns not present at sorting time were dropped from sort order [%s]",
                        paste(.sortdropped, collapse=", ")))
    })

  # record ids
  .data = .data %>% ungroup() %>% mutate(RECID=row_number(ID)) # really cannot get an error here if ID exists

  tryCatch({
    # Default Time Transformations
    ## elapsed time & time after first dose
    .data = .data %>% group_by(ID) %>%
      mutate(ELTM = elapsed.time(TIME),
             TAFD = difftime(TIME,TIME[Position(function(i) i==1, EVID)], units="hours"))
    ## time after dose
    .data = .data %>%  mutate(NDOSE = pmax(1,cumsum(EVID==1))) %>% group_by(ID,NDOSE) %>%
      mutate(TAD = difftime(TIME,TIME[Position(function(i) i==1, EVID)], units="hours"))},
    error = function(cond){
      warning("Post Merge: Unable process default Time Transformations.  ELTM, TAD, TAFD defaults not computed.")
    })

  # user specified transform, filter, exclusion
  tryCatch({
    if(!missing(fun.transform)) .data = fun.transform(.data)},
    error=function(cond){
      warning(sprintf("Post Merge: fun.transform function contains an error\n\t%s", cond$message))
    })
  tryCatch({
    if(!missing(fun.filter)) .data = fun.filter(.data)},
    error=function(cond){
      warning(sprintf("Post Merge: fun.filter contains an error\n\t%s", cond$message))
    })
  tryCatch({
    if(!missing(fun.exclude)) .data = fun.exclude(.data)},
    error=function(cond){
      warning(sprintf("Post Merge: fun.exclude contains an error\n\t%s", cond$message))
    })

  # remove EVID 2, can't get an error here.
  if(!options$KeepEvid2){
    .data = .data %>% filter(!(EVID==2 & is.na(DV)))
    message("Dropped EVID 2 records with DV==NA")
  }

  #
  if(options$DropEmptyColumns){
    is_empty_col = function(x){
      all(is.na(x) | grepl("^\\s*$", x))
    }
    emptycol = purrr::map_lgl(.data,is_empty_col)
    .data = .data %>% select_if(!emptycol)
    message(sprintf("Dropped empty columns [%s]", paste(names(emptycol)[emptycol],collapse=", ")))
  }

  .data %>% select(RECID,everything())
}


#' Pre-merge one domain with another
#'
#' @param dom1 The primary domain object that will contain the result
#' @param dom2 The secondary domain object that will be merged into dom1
#' @param keys The columns to use for merging (character vector)
#' @param .filter A filter to apply to the secondary domain before the merge is completed.
#' @param jointype One of "left", "right", "inner", "full".
#' @param supp Logical indicating if the pre.merge function should treat \code{dom2} as
#' a CDISC SUPPQUAL domain.
#' @param ... Additional named arguments for the columns to include from \code{dom2}  May include
#' tranfsormations. As of version 0.5 all columns from \code{dom2} are included.
#'
#' @details The first domain \code{dom1} is merged with a second domain \code{dom2}.  The
#' second domain is loaded, de novo, and the raw input data is available from the source file
#' for merging with the first domain.  If \code{supp} is TRUE then \code{dom2} will be
#' unstacked on QNAM and QVAL, which must be present in \code{dom2} or supplied in the additional
#' arguments. (\code{...}).
#'
#' @return A tbl_df with the merged data
#' @importFrom lazyeval lazy_dots
#' @importFrom dplyr mutate_ left_join right_join inner_join full_join select_ "%>%" if_else
#' @importFrom tidyr spread_
#' @export
#'

pre.merge = function(dom1, dom2, keys, .filter, jointype="left", supp=F, ...){
  df1 = dom1$Data
  df2 = load.domain(dom2, .fun=NULL, .hook=NULL)$Data # this will not run preprocessing and the domain data will not be kept after this call
  df2.cols = lazyeval::lazy_dots( ...)
  df2.cols = unique(c(keys, names(df2.cols)))
  df2 = df2 %>% mutate_(.dots=lazyeval::lazy_dots(...)) #%>% select_( .dots=df2.cols)
  if(!missing(.filter)) df2 = filter_(df2, substitute(.filter))
  if(supp){
    if(nrow(df2)>0){
      # check for duplicate QNAM and tag QNAM.2, etc before spread.  If  .filter takes out all rows
      # this gets ugly, so keep in check.  Can't have dups with no rows, anyway
      df2 = df2 %>% group_by_(.dots=c(keys,"QNAM")) %>%
        mutate(qnam_cnt=1:n()) %>% ungroup %>%
        mutate(QNAM=if_else(qnam_cnt>1, paste(QNAM, qnam_cnt, sep="_"), QNAM))
      hasdups = max(df2$qnam_cnt)>1
      if(hasdups) {
        msg = paste0("The supplementary domain [%s] is poorly formatted.  It contains ",
                     "duplicate QNAM values, which causes a row mismatch and creation ",
                     "of extra columns.  Duplicate QNAM columns are added with names ",
                     "like QNAM_2, QNAM_3, etc., beyond the first QNAM value. You may ",
                     "wish to pre-process the supplementary domain or just use one of ",
                     "the duplicated columns going forward.")
        warning(sprintf(msg,dom2$name))
      }
    }
    # spread out on QNAM
    df2 = df2 %>% select_(.dots=unique(c("QNAM","QVAL",df2.cols))) %>% spread_("QNAM", "QVAL")
  }

  fun = switch(jointype,
               right = right_join,
               inner = inner_join,
               full = full_join,
               left = , #left and anything else fall through to left_join
               left_join)
  attr.sav=list()
  for(col in keys){
    # keep key attributes
    attr.sav[[col]] = attributes(df1[[col]])
    # remove key attributes
    attributes(df1[[col]])<-NULL
    attributes(df2[[col]])<-NULL
  }

  # run the join
  .out = fun(df1,df2,by=keys)

  for(col in keys){
    # replace key attributes
    attributes(.out[[col]])<-attr.sav[[col]]
  }

  .out
}

#' Apply ADDL and Missing doses to a dosing data frame
#'
#' @param df A tbl_df or data.frame object with all of the dosing data
#' @param expandADDL A logical indicating if ADDL records should be expanded to individual
#' dosing records
#'
#' @return A tbl_df object with the appended data
#'
#' @details This routine looks for pertinent columns in df and determines if information is
#' available to apply ADDL imputation, missing dose exclusion and dose splitting.  For ADDL
#' imputation it looks for an II column.  If the II column is found rows where II>0 indicate the
#' beginning of a repeating dose.  A new dose record is injected into the output after the initial dose
#' to specify the additional doses.  The additional doses will continue until the next dose, stopping one
#' dosing interval before the next dose.
#'
#' For dose splitting, SPLIT should contain a numeric value greater than or equal to 0 in each row.  The
#' dosing rows will be duplicated the specified number of times.  If CMT is given, ".<SPLIT>" will be
#' appended to it (e.g. "1.1", "1.2" or "PARENT.1", "PARENT.2").  IF CMT is not found, it will be created using
#' the name "DOSE", such that the reported value is "DOSE.1" for the first duplicate.
#'
#'
#' @importFrom dplyr mutate select "%>%" arrange_ group_by_
#' @export
#'
post_process_dosing = function(df, expandADDL=F, ADDLTolerance=0.5){
  if(!is.data.frame(df)) stop("argument 'df' must be derivative of type data.frame")
  if(is.null(ADDLTolerance)){ADDLTolerance=0.5}
  # if grouped, capture groups and make sure the same when giving back
  original_groups <- as.character(dplyr::groups(df))

  df_names = names(df)
  if("II" %in% df_names){
    if(!is.numeric(df$II)) stop("II must be numeric")
    message(sprintf("ADDL Tolerance set to %s", ADDLTolerance))
    # impute ADDL
    grouping = df_names[df_names %in% c("ID","CMT")]
    df = df %>% group_by_(grouping) %>%
      #dplyr 0.5 arrange no longer obeys grouping
      #arrange(TIME) %>%
      arrange_(.dots=c(grouping,"TIME")) %>%
      get_addl_dosing(.tolII=ADDLTolerance) %>%
      group_by_(grouping) %>% arrange(TIME)
    if(expandADDL){
      # expand ADDL records
      df = expand_addl(df)
    }
  }

  if (!length(original_groups)) {
    df <- dplyr::ungroup(df)
  } else {
    df <- dplyr::group_by_(df, original_groups)
  }
  df
}
