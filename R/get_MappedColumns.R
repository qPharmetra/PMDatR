


#' Processes domain data for individual doses
#'
#' @param .data The domain data frame
#' @param ID A column name or expression for the subject identifier.
#' @param TIME A column name or expression for the TIME column.
#' @param AMT  A column name or expression for the AMT column.
#' @param EVID A column name or expression for the EVID column (defaults to 1).
#' @param MDV A column name or expression for the MDV column (defaults to 1).
#' @param ex.filter Filter expression to apply to subset the data.
#' @param Units A column name or expression containing the units of AMT.
#' @param ... Additional column mappings.
#'
#' @details The input data is filtered, if a filter expression is provided.  ID, TIME, and AMT are required.  EVID
#' can be provided as a number or expression, but will default to 1 if not supplied.  Additional column mappings
#' will be added to the returned data.
#'
#' This function uses non-standard evaluation, so the arguments do not need to be quoted.  For example, to compute
#' a date from ISO-8601 strings include \code{TIME=parsedate::parse_date(EXSTDTC)}.  Of course the actual column may
#' have a different name.  Extra arguments (...) must be named, for example: MYVALUE = pmax(0,DV).
#'
#' AMT units may be supplied as a column reference (eg, EXDOSU) or as a fixed string (eg, "mg").  If only a single
#' unit is represented then AMT will be converted to a pmunits column, where the units are stored as a column attribute.
#' Otherweise a column AMTU will be created and carried through to the output.
#'
#' @return A data_frame with the mapped data
#' @importFrom lazyeval lazy_dots lazy expr_text
#' @importFrom dplyr filter_ mutate_ select_ arrange do
#' @export
#'

getIndividualDoses = function(.data, ID, TIME, AMT, EVID, MDV, ex.filter, Units, combine.tol=0.0, ...){

  # check inputs for id, time, amt
  if(missing(ID)) stop("getIndividualDoses cannot have missing argument 'ID'")
  if(missing(TIME)) stop("getIndividualDoses cannot have missing argument 'TIME'")
  if(missing(AMT)) stop("getIndividualDoses cannot have missing argument 'AMT'")

  #defaulting of missing parameters
  if(missing(EVID)){EVID=1; message("getIndividualDoses: EVID defaulted to 1")}
  if(missing(MDV)){MDV=1; message("getIndividualDoses: MDV defaulted to 1")}
  # filter the data

  if(!missing(ex.filter)){
    tryCatch({
      .data = filter_(.data, lazy(ex.filter, .follow_symbols=T))
    },
    error=function(cond){stop(sprintf("getIndividualDoses: unable to process ex.filter expression: %s\n\t%s", expr_text(ex.filter), cond$message))},
    warning=function(cond){warning(sprintf("getIndividualDoses: ex.filter expression: %s\n\t%s", expr_text(ex.filter), cond$message))})
    }

  if(nrow(.data)==0){
    warning("getIndividualDoses data has zero rows.  Check ex.filter and input data.  Returning a zero row tibble")
    return(data_frame())
  }

    # check Units for column name or units string
  if(!missing(Units)){
    .units = expr_text(Units)
    if(!.units %in% names(.data)) {
      #it's not a name in the dataframe
      if(udunits2::ud.is.parseable(.units)){
        # it's a parseable unit, so use the string
        Units=.units
      } else {
        # this is a problem, it's not a unit nor a valid name
        warning("getIndividualDoses Units is not a column name or valid unit.  Passing to evaluation and hoping for the best.")
      }
    }
  }

  # Units is either a column with units in it, or it's a units string

  #   this is a clever way to leave out NULL elements
  trans.cols = lazy_dots(ID=ID, TIME=TIME, AMT=AMT, EVID=EVID, AMTU=Units, MDV=MDV, ..., .follow_symbols = T)
  if(missing(Units)) trans.cols[["AMTU"]]=NULL
  if(missing(MDV)) trans.cols[["MDV"]]=NULL

  # here is where the magic happens.  The mappings go into mutate, which can process
  #   simple assignements as well as functions

  tryCatch({
    .data = .data %>% mutate_(.dots=trans.cols) %>%  select_(.dots=names(trans.cols))},
    error = function(cond) stop(sprintf("getIndividualDoses: Error in column transformations:\n\t%s", cond)),
    warning=function(cond)message(sprintf("getIndividualDoses: warning in column transformations:\n\t%s", cond))
  )

  # combine doses, if tol>0
  if(combine.tol>0){
    # need to group.  Look for ID, CMT
    nex = nrow(.data)
    grps = intersect(c("ID","CMT"),names(.data))
    .data = .data %>% group_by_(.dots=grps) %>% arrange(TIME) %>%
      do(combine_rows_by_tol(., "TIME", tol=combine.tol*60, AMT=sum(AMT))) %>%
      ungroup
    nex.new = nrow(.data)
    ## message indicating if/how many doses were combined
    ncomb = nex - nex.new
    message(sprintf(paste0("Dose combination - a total of [%s] dose records ",
                           "were combined using a tolerance of [%.3g] minutes"),
                    ncomb, combine.tol))
  }

  # check for CMT column, and if present, split on commas
  if("CMT" %in% names(.data)) .data = .data %>% separate_rows("CMT")


  .data

}

#' Processes domain data for specified dependent variables (DV)
#'
#' @param .data The domain data frame
#' @param ID A column name or expression for the subject identifier.
#' @param TIME A column name or expression for the TIME column.
#' @param DV A column name or expression for the dependent variable (DV).
#' @param BQL An expression returning TRUE when the observation is BQL.
#' @param LLOQ An expression giving the LLOQ value to used in replacing DV when BQL is TRUE.
#' @param EVID A column name or expression for the EVID column (defaults to 0).
#' @param MDV A column name or expression for the MDV column (defaults to 0).
#' @param dv.filter Filter expression to apply to subset the data.
#' @param Units A column name or expression containing the units of DV.
#' @param ... Additional column mappings.
#'
#' @details The input data is filtered, if a filter expression is provided.  ID, TIME, and DV are required.  If BQL and
#' LLOQ are supplied, then DV will be set to the LLOQ value when BQL is TRUE.  EVID can be provided as a number or
#' expression, but will default to 0 if not supplied.  Additional column mappings will be added to the returned data.
#'
#' This function uses non-standard evaluation, so the arguments do not need to be quoted.  For example, to compute
#' a date from ISO-8601 strings include \code{TIME=parsedate::parse_date(EXSTDTC)}.  Of course the actual column may
#' have a different name.  Extra arguments (...) must be named, for example: MYVALUE = pmax(0,DV)
#'
#' @return A data_frame with the mapped data
#' @importFrom lazyeval lazy_dots lazy
#' @importFrom dplyr filter_ mutate_ select_ mutate
#' @export
#'

getDV = function(.data, ID, TIME, DV, BQL, LLOQ, EVID, MDV, dv.filter, Units, ...){

  # check inputs for id, time, dv
  if(missing(ID)) stop("getDV cannot have missing argument 'ID'")
  if(missing(TIME)) stop("getDV cannot have missing argument 'TIME'")
  if(missing(DV)) stop("getDV cannot have missing argument 'DV'")

  #defaulting of missing parameters
  if(missing(EVID)){EVID=0; message("getDV: EVID defaulted to 0")}
  if(missing(MDV)){MDV=0; message("getDV: MDV defaulted to 0")}

  # filter first

  if(!missing(dv.filter)){
    tryCatch({
      .data = filter_(.data, lazy(dv.filter, .follow_symbols=T))
    },
    error=function(cond){stop(sprintf("getDV: unable to process dv.filter expression: %s\n\t%s", expr_text(dv.filter), cond$message))},
    warning=function(cond){warning(sprintf("getDV: dv.filter expression: %s\n\t%s",, expr_text(dv.filter), cond$message))})
  }

  if(nrow(.data)==0){
    warning("getDV: data has zero rows.  Check dv.filter and input data.  Returning a zero row tibble")
    return(data_frame())
  }

  # check Units for column name or units string
  if(!missing(Units)){
    .units = expr_text(Units)
    if(!.units %in% names(.data)) {
      #it's not a name in the dataframe
      if(udunits2::ud.is.parseable(.units)){
        # it's a parseable unit, so use the string
        Units=.units
      } else {
        # this is a problem, it's not a unit nor a valid name
        warning("getDV Units is not a column name or valid unit.  Passing to evaluation and hoping for the best.")
      }
    }
  }

  # combine the arguments into a list for lazy evaluation
  trans.cols = lazy_dots(ID=ID, TIME=TIME, DV=DV, DVU=Units, EVID=EVID, BQL=BQL, LLOQ=LLOQ, MDV=MDV, ..., .follow_symbols=T)
  if(missing(BQL)) trans.cols[["BQL"]]=NULL
  if(missing(LLOQ)) trans.cols[["LLOQ"]]=NULL
  if(missing(Units)) trans.cols[["DVU"]]=NULL
  if(missing(MDV)) trans.cols[["MDV"]]=NULL

  # here is where the magic happens.  The mappings go into mutate, which can process
  #   simple assignements as well as functions
  if(!is.null(trans.cols)){
    tryCatch({
      .data = .data %>% mutate_(.dots=trans.cols) %>% select_(.dots=names(trans.cols))},
      error = function(cond) stop(sprintf("getDV: Error in column transformations:\n\t%s", cond)),
      warning=function(cond)message(sprintf("getDV: warning in column transformations:\n\t%s", cond))
    )
  }

  # overwrite DV for BQL if LLOQ was provided
  if(!missing(BQL) & !missing(LLOQ)){
    if(!inherits(.data$BQL,"logical"))
      stop(sprintf("getDV BLQ mapping '%s' does not evaluate to TRUE/FALSE",
                   deparse(trans.cols$BQL$expr)))
    if(!inherits(.data$LLOQ,"numeric"))
      stop(sprintf("getDV LLOQ mapping '%s' results in non-numeric values",
                   deparse(trans.cols$LLOQ$expr)))
    # if we're still alive, compute DV for the BQL records
    .data[.data$BQL,] = mutate(.data[.data$BQL,], DV=LLOQ)

  }

  .data

}


#' Processes domain data for merged covariates
#'
#' @param .data The domain data frame
#' @param ID A column name or expression for the subject identifier
#' @param cov.col The name of the column that holds the desired covariate names
#' @param cov.val The name of the column that holds the covariate values
#' @param cov.filter Filter expression to apply to subset the data
#' @param cov.keys A character vector of the names of the columns to use as merge keys
#' @param fun.summary A function name (or named list/vector) as a text string to use in summarising the ... column mappings over duplicate cov.keys
#' @param Units The name of the column holding the covariate units (if stacked).
#' @param ... Additional column mappings.
#'
#' @details The input data is filtered, if a filter expression is provided.  ID must be specified, but other arguments are optional.
#' Provide cov.col and cov.val both to indicate that the data are in long format.  In this case the values in cov.col
#' (that remain after the filter is applied) will become
#' new columns with the values specified in cov.val (a data widening operation).  The ID and cov.keys columns will be repeated
#' for each covariate value row.
#'
#' If cov.col and cov.val are not provided, then after filtering the expressions given as additional column mappings will
#' be selected from .data
#'
#' cov.keys is used to group the output data for merging.  Keys must be supplied.  If multiple rows are returns per
#' cov.keys grouping, a warning will be generated.  Pass a function name in fun.summary to summarise values within
#' each group.  For example, there may be replicated samples and we want the mean of the samples for each subject and visit.
#'
#' This function uses non-standard evaluation, so the ... arguments do not need to be quoted.  For example, to compute
#' a date from ISO-8601 strings include \code{TIME=parsedate::parse_date(EXSTDTC)}.  Of course the actual column may
#' have a different name.  Extra arguments (...) must be named, for example: \code{MYVALUE = pmax(0,DV)}.
#'
#' @return A data_frame with the mapped data
#' @importFrom lazyeval lazy_dots interp lazy
#' @importFrom reshape2 dcast
#' @importFrom dplyr filter_ mutate_ select_ group_by_ summarise_each_
#' @importFrom tidyr spread
#' @importFrom stats setNames
#' @export
#'

getCov = function(.data, ID, cov.col, cov.val, cov.filter, cov.keys, fun.summary, Units, ...){

  # check inputs
  if(missing(ID)) stop("getCov cannot have missing argument 'ID'")
  if(!(missing(cov.col) == missing(cov.val))) stop("getCov - both or neither of cov.col and cov.val must be specified")
  if(missing(cov.keys)) stop("getCov - merge keys must be supplied in cov.keys")

  # process fun.summary - complicated
  if(!missing(fun.summary)){
    #summary only matters if we end up with duplicate keys after filtering and processing.  For now we can check that
    # we were given the correct type of info for it
    # if it's a named character  make it a list
    if(is.character(fun.summary) & length(names(fun.summary)>0)) fun.summary=as.list(fun.summary)
    # if it's length>1 make sure it has valid names
    if(length(fun.summary)>1){
      sum.names = names(fun.summary)
      valid.names = is_valid_variable_name(sum.names)
      valid.names= sum.names[valid.names]
      if(length(valid.names)<length(sum.names)){
        warning("getCov - dropping unnamed/invalidly named fun.summary elements.  Will use default summary for unspecified columns.")
        fun.summary = fun.summary[valid.names]
      }
    }
  }


  # filter the data

  if(!missing(cov.filter)){
    tryCatch({
      .data = filter_(.data, lazy(cov.filter, .follow_symbols=T))
    },
    error=function(cond){stop(sprintf("getCov: unable to process cov.filter expression: %s\n\t%s", expr_text(cov.filter), cond$message))},
    warning=function(cond){warning(sprintf("getCov: cov.filter expression: %s\n\t%s",expr_text(cov.filter), cond$message))})
  }

  if(nrow(.data)==0){
    warning("getCov: data has zero rows.  Check cov.filter and input data.  Returning a zero row tibble")
    return(data_frame())
  }

  # set up args for lazy evaluations
  trans.cols = lazy_dots(ID=ID,  ..., .follow_symbols = T)
  # evaluate the key column transformations.  This will fail if the key columns involve stacked param code values
    ## but seriously, why would they?

  if(!is.null(trans.cols)){
    tryCatch({
      .data = .data %>% mutate_(.dots=trans.cols[cov.keys])
    },
    error = function(cond) stop(sprintf("getCov: Error in column transformations:\n\t%s", cond)),
    warning=function(cond)message(sprintf("getCov: warning in column transformations:\n\t%s", cond))
    )
  }

  # to not remove duplicates unitl summarization, group over keys and cov.col and then add a counter column
  ## the point here is that spread doesn't like duplicated keys, so we need duplicates on separate rows
  ## but then we need to get rid of the duplicates in a summary step or joining the covariates to the events
  ## will duplicate events (bad!)
  dupgroups = c(cov.keys)
  if(!missing(cov.col)) dupgroups=c(dupgroups,lazyeval::expr_text(cov.col))
  .data = .data %>% group_by_(.dots=dupgroups) %>%
    mutate(N_DUP_=1:dplyr::n()-1) %>% ungroup

  # if long data, then first convert to wide
  if(!missing(cov.col)){
    keycol = deparse(substitute(cov.col))
    valcol = deparse(substitute(cov.val))
    unitcol = deparse(substitute(Units))
    # filter by the required columns
    var.names = unlist(lapply(lazy_dots(ID=ID, ..., .follow_symbols = T), function(x) all.vars(x$expr)))
    var.names = unique(c(var.names, cov.keys, unitcol))
    .data = .data %>% filter_(interp(~ col %in% codes, col=as.name(keycol), codes=c(var.names)))

    # select necessary columns
    selcols = c(var.names[var.names %in% names(.data)], keycol, valcol, "N_DUP_")
    .data = .data  %>%  select_(.dots = selcols)
    if(missing(Units)) .data = .data %>%  tidyr::spread_(keycol,valcol)
    else  .data = .data %>% PMDatR::spread_(keycol,valcol,.units_col=unitcol)

  }


  # The mappings go into mutate, which can process
  #   simple assignements as well as functions
  if(!is.null(trans.cols)){
    tryCatch({
      .data = .data %>% mutate_(.dots=trans.cols[setdiff(names(trans.cols),cov.keys)])
      },
      error = function(cond) stop(sprintf("getCov: Error in column transformations:\n\t%s", cond)),
      warning=function(cond)message(sprintf("getCov: warning in column transformations:\n\t%s", cond))
    )
  }

  # check for cov.keys after transformation
  if(!all(cov.keys %in% names(.data))){
    stop(paste0("getCov - merge keys (", cov.keys[!(cov.keys %in% names(.data))] ,") must be present in .data or as a mapped column"))
  }

  # retain only the columns that are keys or covariate mappings
  .data =.data %>%select_(.dots=unique(c(cov.keys,names(trans.cols))))

  # check for duplicates and summarise if we find any
  .dups = .data %>% group_by_(.dots=cov.keys) %>% filter(dplyr::n()>1)
  if(nrow(.dups)>0){
    # we're going to summarise over the keys (we just did the grouping, so should be good to go there)
    message("getCov - Some rows have duplicated merge keys.  Summary function(s) will be applied to remove duplicate rows.")

    #start with defaults then override
    col.summ = setNames(as.list(rep("first_",ncol(.data))),names(.data))


    if(missing(fun.summary)){
      warning("getCov - fun.summary defaulted to 'first_'")
    }
    else{ # this is a little bit ugly
      if(is.character(fun.summary)){
        #must be single value or would have been converted to list
        col.summ=lapply(col.summ,function(x) x=fun.summary)
        # default for non-numeric column is first_
        col.summ[!sapply(.data,is.numeric)]="first_"
        }
      else
        if(is.list(fun.summary)){
          # we're more lenient with the list.  User can override character columns with last_, for instance.
          col.summ = modifyList(col.summ, fun.summary)
          }
          else{warning("getCov - unrecognized format in fun.summary, using default summary function for all columns")}
    }


    # remove keys
    col.summ[cov.keys]=NULL
    message(sprintf("getCov - Summarizing with functions: %s",
                    paste0(unlist(lapply(seq_along(col.summ),
                                         function(x) sprintf("%s=%s", names(col.summ)[x], col.summ[[x]]))),collapse = ", ")))
    # we have the columns, now convert to lazy eval and run the summary
    lnams=names(col.summ) # just for shorthand in the next call
    #caller.env = parent.frame()
    summ.l = setNames(lapply(seq_along(col.summ),
                             function(i) lazyeval::interp(lazyeval::lazy(func(col)),# env=caller.env),
                                                          func=as.name(col.summ[[i]]),
                                                          col=as.name(lnams[i]))),
                      lnams)
    # capture units before summarise_
    .units = unit_cols(.data)
    .data = .data %>% group_by_(.dots=cov.keys) %>% summarise_(.dots=summ.l)
    # if we found units, put them back
    if(!is.null(.units)) .data = set_units_from_list(.data,.units)

  }
  .data %>% group_by_(.dots=cov.keys)
}


#' Processes domain data for time-varying covariates
#'
#' @param .data The domain data frame
#' @param ID A column name or expression for the subject identifier.
#' @param TIME A column name or expression for the TIME column.
#' @param EVID A column name or expression for the EVID column.
#' @param covT.col The name of the column that holds the desired covariate names
#' @param covT.val The name of the column that holds the covariate values
#' @param covT.filter Filter expression to apply to subset the data.
#' @param fun.summary A function name (or named list/vector) as a text string to use if more than one value per (ID, TIME) is encountered.
#' @param Units The name of the column holding the covariate units (if stacked).
#' @param ... Additional column mappings.
#'
#' @details The input data is filtered, if a filter expression is provided.  ID and TIME must be specified, but other arguments are optional.
#' If EVID is not provided, it will default to '2'.  Provide cov.col and cov.val both to indicate that the data are in long format.  In this case the values in cov.col
#' (that remain after the filter is applied) will become
#' new columns with the values specified in cov.val (a data widening operation).  The ID and TIME columns will be repeated
#' for each covariate value row.
#'
#' If covT.col and covT.val are not provided, then after filtering the expressions given as additional column mappings will
#' be selected from .data
#'
#' This function uses non-standard evaluation, so the ... arguments do not need to be quoted.  For example, to compute
#' a date from ISO-8601 strings include \code{TIME=parsedate::parse_date(EXSTDTC)}.  Of course the actual column may
#' have a different name.  Extra arguments (...) must be named, for example: \code{MYVALUE = pmax(0,DV)}.
#'
#' @return A data_frame with the mapped data
#' @importFrom lazyeval lazy_dots lazy
#' @importFrom dplyr filter_ mutate_ select_ as_data_frame
#' @importFrom stats setNames
#' @export
#'

getCovT = function(.data, ID, TIME, EVID, covT.col, covT.val, covT.filter, fun.summary, Units, ...){

  # check inputs for id, time
  if(missing(ID)) stop("getCovT cannot have missing argument 'ID'")
  if(missing(TIME)) stop("getCovT cannot have missing argument 'TIME'")
  if(!(missing(covT.col) == missing(covT.val))) stop("getCovT - both or neither of covT.col and covT.val must be specified")

  # filter the data

  if(!missing(covT.filter)){
    tryCatch({
      .data = filter_(.data, lazy(covT.filter, .follow_symbols=T))
    },
    error=function(cond){stop(sprintf("getCovT: unable to process covT.filter expression: %s\n\t%s", expr_text(covT.filter), cond$message))},
    warning=function(cond){warning(sprintf("getCovT: covT.filter expression: %s\n\t%s",expr_text(covT.filter), cond$message))})
  }

  if(nrow(.data)==0){
    warning("getCovT: data has zero rows.  Check covT.filter and input data.  Returning a zero row tibble")
    return(data_frame())
  }

  #defaulting of missing parameters
  if(missing(EVID)){EVID=2; message("getCovT: EVID defaulted to 2")}

  # process fun.summary - complicated
  if(!missing(fun.summary)){
    #summary only matters if we end up with duplicate keys after filtering and processing.  For now we can check that
    # we were given the correct type of info for it
    # if it's a named character  make it a list
    if(is.character(fun.summary) & length(names(fun.summary)>0)) fun.summary=as.list(fun.summary)
    # if it's length>1 make sure it has valid names
    if(length(fun.summary)>1){
      sum.names = names(fun.summary)
      valid.names = is_valid_variable_name(sum.names)
      valid.names= sum.names[valid.names]
      if(length(valid.names)<length(sum.names)){
        warning("getCov - dropping unnamed/invalidly named fun.summary elements.  Will use default summary for unspecified columns.")
        fun.summary = fun.summary[valid.names]
      }
    }
  }

  # set up args for lazy evaluations
  trans.cols = lazy_dots(ID=ID, TIME=TIME, EVID=EVID,  ..., .follow_symbols = T)

  ## set cov.keys to ID and TIME
  cov.keys=c("ID","TIME")

  # evaluate the key column transformations.  This will fail if the key columns involve stacked param code values
  ## but seriously, why would they?

  if(!is.null(trans.cols)){
    tryCatch({
      .data = .data %>% mutate_(.dots=trans.cols[cov.keys])
    },
    error = function(cond) stop(sprintf("getCovT: Error in column transformations:\n\t%s", cond)),
    warning=function(cond)message(sprintf("getCovT: warning in column transformations:\n\t%s", cond))
    )
  }

  # to not remove duplicates unitl summarization, group over keys and cov.col and then add a counter column
  ## the point here is that spread doesn't like duplicated keys, so we need duplicates on separate rows
  ## but then we need to get rid of the duplicates in a summary step or joining the covariates to the events
  ## will duplicate events (bad!)
  dupgroups = c(cov.keys)
  if(!missing(covT.col)) dupgroups=c(dupgroups,lazyeval::expr_text(covT.col))
  .data = .data %>% group_by_(.dots=dupgroups) %>%
    mutate(N_DUP_=1:dplyr::n()-1) %>% ungroup

  # if long data, then first convert to wide
  if(!missing(covT.col)){
    keycol = deparse(substitute(covT.col))
    valcol = deparse(substitute(covT.val))
    unitcol = deparse(substitute(Units))
    # filter by the required columns
    var.names = unlist(lapply(lazy_dots(ID=ID, TIME=TIME, EVID=EVID, ..., .follow_symbols = T), function(x) all.vars(x$expr)))
    var.names = unique(c(var.names, cov.keys, unitcol))
    .data = .data %>% filter_(interp(~ col %in% codes, col=as.name(keycol), codes=c(var.names)))
    # select necessary columns
    selcols = c(var.names[var.names %in% names(.data)], keycol, valcol, "N_DUP_")
    .data = .data  %>%  select_(.dots = selcols)
    if(missing(Units)) .data = .data %>%  tidyr::spread_(keycol,valcol)
    else  .data = .data %>% PMDatR::spread_(keycol,valcol,.units_col=unitcol)
  }


  # The mappings go into mutate, which can process
  #   simple assignements as well as functions
  # already processed the key columns so don't do those
  if(!is.null(trans.cols)){
    tryCatch({
      .data = .data %>% mutate_(.dots=trans.cols[setdiff(names(trans.cols),cov.keys)])
    },
    error = function(cond) stop(sprintf("getCovT: Error in column transformations:\n\t%s", cond)),
    warning=function(cond)message(sprintf("getCovT: warning in column transformations:\n\t%s", cond))
    )
  }

  # retain only the columns that are keys or covariate mappings
  .data =.data %>%select_(.dots=unique(c(cov.keys,names(trans.cols))))

  # check for duplicates and summarise if we find any
  .dups = .data %>% group_by_(.dots=cov.keys) %>% filter(dplyr::n()>1)
  if(nrow(.dups)>0){
    # we're going to summarise over the keys (we just did the grouping, so should be good to go there)
    message("getCovT - Some rows have duplicated ID and TIME.  Summary function(s) will be applied to remove duplicate rows.")

    #start with defaults then override
    col.summ = setNames(as.list(rep("first_",ncol(.data))),names(.data))
    # default for non-numeric column is first_
    col.summ[!sapply(.data,is.numeric)]="first_"
    # replace EVID
    col.summ["EVID"]="first_"

    if(missing(fun.summary)){
      warning("getCov - fun.summary defaulted to 'first_'")
    }
    else{ # this is a little bit ugly
      if(is.character(fun.summary)){
        #must be single value or would have been converted to list
        col.summ=lapply(col.summ,function(x) x=fun.summary)
        # default for non-numeric column is first_
        col.summ[!sapply(.data,is.numeric)]="first_"
      }
      else
        if(is.list(fun.summary)){
          # we're more lenient with the list.  User can override character columns with last_, for instance.
          col.summ = modifyList(col.summ, fun.summary)
        }
      else{warning("getCovT - unrecognized format in fun.summary, using default summary function for all columns")}
    }


    # remove keys
    col.summ[cov.keys]=NULL

    message(sprintf("getCovT - Summarizing with functions: %s",
                    paste0(unlist(lapply(seq_along(col.summ),
                                         function(x) sprintf("%s=%s", names(col.summ)[x], col.summ[[x]]))),collapse = ", ")))
    # we have the columns, now convert to lazy eval and run the summary
    lnams=names(col.summ) # just for shorthand in the next call
    #caller.env = parent.frame()
    summ.l = setNames(lapply(seq_along(col.summ),
                             function(i) lazyeval::interp(lazyeval::lazy(func(col)),# env=caller.env),
                                                          func=as.name(col.summ[[i]]),
                                                          col=as.name(lnams[i]))),
                      lnams)

    # capture units before summarise_
    .units = unit_cols(.data)
    .data = .data %>% group_by_(.dots=cov.keys) %>% summarise_(.dots=summ.l)
    # if we found units, and they're not still in .data, put them back
    if(!is.null(.units) & is.null(unit_cols(.data))) .data = set_units_from_list(.data,.units)

  }

  .data

}


#' Processes source data
#'
#' @param .data The domain data frame
#' @param .filter Filter expression to apply to subset the data.
#' @param ... Additional column mappings.
#'
#' @details The input data is filtered, if a filter expression is provided. Named parameters provided
#' in the extra arguments are evaluated and selected from the input dataset.
#'
#' This function uses non-standard evaluation, so the ... arguments do not need to be quoted.  For example, to compute
#' a date from ISO-8601 strings include \code{TIME=parsedate::parse_date(EXSTDTC)}.  Of course the actual column may
#' have a different name.  Extra arguments (...) must be named, for example: \code{MYVALUE = pmax(0,DV)}.
#'
#' @return A data_frame with the mapped data
#' @importFrom lazyeval lazy_dots
#' @importFrom dplyr filter_ mutate_ select_ as_data_frame
#' @export

getDomain = function(.data, .filter, ...){

  #error handling
  add.error = function(x){errs<<-c(errs,x)}
  errs = list()

  #   this is a clever way to leave out NULL elements
  trans.cols = lazy_dots(..., .follow_symbols = T)

  # symantic check of inputs
  data.names = names(.data)
  dots.names = as.list(substitute(list(...)))[-1]

  if(!missing(.filter)) {
    not.in.data = !(all.vars(substitute(.filter)) %in% data.names)
    if(!all(not.in.data))
    {.data = filter_(.data, lazy(.filter, .follow_symbols=T))} #filter_(.data, substitute(.filter))}
    else {
      warning(sprintf("D101: filter cannot be run because columns are not in data: %s",
              paste(all.vars(substitute(.filter))[not.in.data], sep=", ")))
    }
  }

  if(length(trans.cols)>0){
    # check the extra columns, getting rid of mappings we don't have columns for
    # we have to check the datanames, but also the previous dots.names
    #keep.dots = lapply(dots.names, function(x){all(all.vars(x)  %in% data.names)})
    keep.dots=logical()
    trans.cols.names = names(trans.cols)
    for(i in 1:length(dots.names)){
      keep.dots[i] = all(all.vars(dots.names[[i]]) %in% data.names)
      data.names = c(data.names,trans.cols.names[i]) #add name of this dots arg to the list of available names
    }


    trans.cols = trans.cols[unlist(keep.dots)]
    # here is where the magic happens.  The mappings go into mutate, which can process
    #   simple assignements as well as functions


    .data=.data %>% mutate_(.dots=trans.cols) %>%  select_(.dots=names(trans.cols))
  }
  .data

}
