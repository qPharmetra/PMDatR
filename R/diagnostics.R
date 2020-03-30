#' ## Diagnostics
#' ## diagnostic functions should return a list of objects to be printed.  For example, a list of character messages.
#' ##   or a data.frame
#'
#' Run diagnostics on a loaded domain object
#'
#' @param dom
#'
#' @return a list of diagnostics.
#' @export
#'

diagnostics.domain = function(dom){
  if(dom$Loaded)
  {
    diagnostics = list()

    # check unique IDs
    diagnostics$nSubjects=sprintf("Number of unique subjects: %s", nsubjects.domain(dom))

    # check for duplicate keys
    diagnostics$duplicate_keys = check_duplicate_keys(dom$Data, dom$InputMappings$UniqueRecordKeys)

    # check numeric columns for text (NA is OK)
    badNums=names(Filter(Negate(validNumeric), dom$Data))
    if(length(badNums)>0)
      diagnostics$badNumeric=sprintf("The following columns are typed as Numeric, but have nonnumeric text entries: %s\n\n",
                                     paste(badNums, collapse=", "))

    # unique values of parameter with different units
    #if(!is.empty.yaml(inp$Parameter) & !is.empty.yaml(inp$Units) not doing units yet

    # missing required columns
    haveCols = names(dom$Data)
    reqCols = required_columns(dom$InputMappings$Columns)
    missCols = reqCols[!reqCols %in% haveCols]
    if(length(missCols)>0)
      diagnostics$missingCols=sprintf("The following required columns are missing in the loaded data: %s\n\n",
                                      paste(missCols, collapse=", "))

    # check date-time columns
    dtcols = datetime_columns(dom$InputMappings$Columns)
    # verify the columns are in the data
    dtcols = intersect(dtcols, names(dom$Data))
    if(length(dtcols)>0)
    {
      date.checks = lapply(dtcols, function(x) check_iso_date_formats(dom$Data[[x]]))
      date.checks=do.call(bind_rows,date.checks) %>% mutate(Column=dtcols) %>% select(Column,everything())
      diagnostics$DateTime$Checked = sprintf("The following columns typed as DateTime are checked for consistency, and any problems are listed below: %s",
                                             paste(date.checks$Column, collapse=", "))
      if(!all(date.checks$all.iso))
        diagnostics$DateTime$NotIso = sprintf("Column(s) have entries that are not fully ISO 8601 formatted: %s",
                                              paste(date.checks$Column[!date.checks$all.iso], collapse=", "))
      if(!all(date.checks$all.date))
        diagnostics$DateTime$FullDates = sprintf("Column(s) have entries lacking complete dates (YYYY-MM-DD): %s",
                                                 paste(date.checks$Column[!date.checks$all.date], collapse=", "))
      if(!all(date.checks$all.time))
        diagnostics$DateTime$AllTime = sprintf("Column(s) have entries with varying/inconsistent time formats (some have times, some do not): %s",
                                               paste(date.checks$Column[!date.checks$all.time], collapse=", "))
      if(!all(date.checks$all.time.nonzero))
        diagnostics$DateTime$AllTimeNonzero = sprintf("Column(s) have entries where all times are given as zero (00:00 or 24:00): %s",
                                                      paste(date.checks$Column[!date.checks$all.time.nonzero], collapse=", "))
    }


    # return diagnostic messages list
    return(diagnostics)
  }

  return(NULL)
}

validNumeric = function(x){
  # make sure we have columns from input mappings
  att = attributes(x)
  if(!is.null(att$type))
  {
    if(att$type=="Numeric")
    {
      valid = is.number(x) | is.na(x)
      return(all(valid))
    }
  }
  return(NA)
}

nsubjects.domain = function(dom){
  id=dom$InputMappings$DefaultQuerySettings$ID
  if(dom$Loaded & !is.empty.yaml(id)){
    return(length(unique(dom$Data[[id]])))
  } else return(NA)
}

required_columns = function(cols){
  # finds T or missing Required elements and returns their names
  sapply(Filter(function(x) x$Required==T, cols), '[[', "Name")
}

datetime_columns = function(cols){
  sapply(Filter(function(x) x$Type=="DateTime", cols), '[[', "Name")
}

regexp_to_list = function(text, regmatch){
  positive <- regmatch != -1
  g_text <- text#[positive]
  g_start <- attr(regmatch, "capture.start")#[positive, , drop = FALSE]
  g_length <- attr(regmatch, "capture.length")#[positive, , drop = FALSE]

  #na.val = setNames(c(NA,NA,NA), c("year","date","time"))

  lapply(seq_along(text), function(i) {
    if(is.na(positive[i])) return(NA)
    if(!positive[i]) return(NA)
    setNames(substring(g_text[i], g_start[i,], g_start[i,] + g_length[i,] - 1),names(g_start[i,]))
  })
}


check_iso_date_formats = function(x, ignore_blanks=T, ignore_na=T){
  #this function checks iso formatted dates for consistency.  Simply answers "are all the formats ISO and the same"
  if(inherits(x,"POSIXct")) x = anytime::iso8601(x)
  #figure which entries to ignore
  ignore = c(which(is.na(x) & ignore_na), which(x=="" & ignore_blanks))
  if(length(ignore)>0) x=x[-ignore]
  # if everything is ignored are length is zero
  if(!length(x)>0) return(data_frame(all.iso=F,all.date=F,all.time=F,all.time.nonzero=F))


  # get the iso format captures in the regex
  iso_regex =  paste0(
    "^\\s*",
    "(?<year>[\\+-]?\\d{4}(?!\\d{2}\\b))",
    "(?<restafteryear>(?<dash>-?)",
    "(?<dateafteryear>(?<month>0[1-9]|1[0-2])",
    "(?<dashandday>\\g{dash}(?<day>[12]\\d|0[1-9]|3[01]))?",
    "|W(?<week>[0-4]\\d|5[0-3])(?<pmweekday>-?(?<weekday>[1-7]))?",
    "|(?<yearday>00[1-9]|0[1-9]\\d|[12]\\d{2}|3",
    "(?<yeardaytail>[0-5]\\d|6[1-6])))",
    "(?<time>[T\\s](?<hourminfrac>(?<hourmin>(?<hour>[01]\\d|2[0-3])",
    "(?<colonmin>(?<colon>:?)(?<min>[0-5]\\d))?|24\\:?00)",
    "(?<frac>[\\.,]\\d+(?!:))?)?",
    "(?<colonsec>\\g{colon}(?<sec>[0-5]\\d)(?<secfrac>[\\.,]\\d+)?)?",
    "(?<tz>[zZ]|(?<tzpm>[\\+-])",
    "(?<tzhour>[01]\\d|2[0-3]):?(?<tzmin>[0-5]\\d)?)?)?)?$"
  )
  capts=regexpr(iso_regex,x,perl = T)
  #convert to list
  captures = regexp_to_list(x,capts)
  # convert to tbl and transmute
  res=do.call(rbind,captures) %>% data.frame() %>% as_data_frame() %>%
    transmute(iso=!is.na(year),
              has.date=(dateafteryear!="" & year!=""),
              has.time=hourmin!="",
              time.zero=hourmin %in% c("00:00","24:00")) %>%
    summarise(all.iso=all(iso),
              all.date=all(has.date),
              all.time=all(has.time) | !any(has.time), # either all T or all F
              all.time.nonzero=!all(time.zero))
  res
}

check_duplicate_keys = function(.df, keys){
  if(!is.empty.yaml(keys)){
    # keys needs to be comma delimited text
    keys = unlist(strsplit(keys,"\\s*,\\s*"))
    .dups = .df %>% group_by_(.dots=keys) %>% summarise(n_dups = n()) %>% filter(n_dups>1)
    if(nrow(.dups)>0){
      return(list(message="Duplicate entries detected.  See the following table (up to first 10 duplicate values):",
                  table = kable(head(.dups, 10))))
    } else{
      return(sprintf("No duplicate entries detected for keys: %s", paste(keys,collapse=",")))
    }
  } else {
    return("No keys specified for duplicate entry detection")
  }
}

#' Pre-merge diagnostic checks
#'
#' These checks occur after the domains are loaded and before the queries are processed.  The goal is to inform on
#' problems that may be found before attempting to run the query processing and merging.
#'
#' @param DM
#'
#' @return A list of diagnostic messages
#' @export
#'

diagnostics_premerge = function(DM)
{
  # the domains are loaded, query processing is not yet run

  diagnostics=list()

  # check for ID mismatches between loaded domains
  missingIDs = find_mismatched_IDs(DM)

  # check for mismatched TRT columns across domains
  # get a dataframe with TRT* columns.
  mismatchedTRT=c(diagnostics, find_mismatched_TRT(DM))

  # return messages
  list(missingIDs=missingIDs, mismatchedTRT=mismatchedTRT)
}

find_mismatched_IDs = function(DM){
  # get a list of IDs in each domain
  domIDs = lapply(DM$Domains, function(x) ID=get_domain_subjects(x))
  # for every combination of domains, get the set difference and return as list
  domIDs = lapply(combn(domIDs,m = 2,simplify=F),
                  function(x) {
                    difx = setdiff(x[[1]],x[[2]])
                    if(length(difx)==0) return(NULL)
                    sprintf("%s not in %s: %s", names(x[1]), names(x[2]), paste(difx, collapse=", "))
                  })
  # knock out NULLs to only preserve messages where we see mismatches
  domIDs=domIDs[!sapply(domIDs,is.null)]
  if(length(domIDs)==0) domIDs="No mismatched IDs found."

}

#' Find mismatched Treatment codes across domains
#'
#' Compares the treatment codes in <master> to those in all other domains.  Treatment code combinations not found
#' in <master> domain are returned in a table.  Searches any columns that begin with TRT.
#'
#' @param DM A data management object
#' @param master The name of the master domain as character
#'
#' @return A tbl containing the unmatched treatment columns and codes by
#' @importFrom tidyr gather
#' @importFrom dplyr select distinct setdiff mutate_
#' @export
#'

find_mismatched_TRT = function(DM, master="DM"){
  # error out if we can't find master domain in doms
  if(!master %in% names(DM$Domains)) stop(sprintf("master domain '%s' does not exist in the DataManagement object", master))

  # return a message if no treatment domains found
  if(DM$Domains[[master]]$Data %>% select(starts_with("TRT")) %>% names %>% length ==0)
  {
    warning(sprintf("No treatment columns found in master domain '%s'",master))
    return(data_frame())
    }

  # get a list of dataframes by domain for ID with unique TRT

  IDTRT = lapply(DM$Domains,
                 function(x) {
                   idcol = x$InputMappings$DefaultQuerySettings$ID
                   if(!all(idcol %in% names(x$Data))){
                     warning(sprintf("Domain '%s' Default Query Settings ID '%s' is not in the domain dataset.  Skipping this domain.",
                                     x$name, idcol))
                     return(NULL)
                   }
                   x$Data %>% select(one_of(idcol), starts_with("TRT")) %>%
                     gather(TRT, TRTCODE, -one_of(idcol)) %>% distinct()

                 })

  # Combine tables, putting domain into each
  tbls = IDTRT
  tbls[[master]]<-NULL

  badTRT = lapply(seq_along(tbls), function(x){
     if(!identical(names(tbls[[x]]), names(IDTRT[[master]]))) return(NULL) #null if we don't have same names = no TRT codes
     df = setdiff(tbls[[x]],IDTRT[[master]])
     if(nrow(df)==0) return(NULL)
     return(df %>% mutate_(.dots=setNames(list(~names(tbls)[x]),"DOMAIN")))
  })
  # give back only not null results
  badTRT[!sapply(badTRT,is.null)]
  do.call(bind_rows,badTRT)
}


get_domain_subjects = function(dom){
  id=dom$InputMappings$DefaultQuerySettings$ID
  if(dom$Loaded & !is.empty.yaml(id)){
    return(unique(dom$Data[[id]]))
  } else return(NA)
}


## Post-Merge diagnostics


#' Summarise observations by EVID in NONMEM dataset
#' @param .data data
#' @param MAP map
#' @param ID a character value giving the column containing subject IDs
#'
#' @return A dataframe
#' @importFrom dplyr group_by_ summarise
#' @export
table_dv = function(.data, strata="", ID="ID")
{
  if(ID=="") ID="ID"
  #.data = apply.map(.data, map = MAP)
  grps = c("EVID",tokenize(strata))
  result = .data %>% group_by_(.dots=grps) %>%
    mutate_(.dots=setNames(list(ID),list("id"))) %>%
    summarise(n_subjects = lunique(ID),
              n_observations=length(DV),
              observation_range=special.range(DV),
              amt_range=special.range(AMT)
    )
  list(tab=result)
}

#' Summarise time variables in NONMEM dataset
#'
#' @param .data data
#' @param MAP map
#' @param tad.tolerance time after dose tolerance
#'
#' @return A data.frame
#' @importFrom tidyr gather_
#' @importFrom dplyr group_by_ summarise intersect
#' @export
#'
table_time = function(.data
                      , strata=""
                      , tad.tolerance=674)
{
  time_cols = intersect(c("TIME","TAD","TAFD","ELTM"), names(.data))
  if(length(time_cols)==0){
    message("No time columns were identified in the dataset.  Looking for one or more of: TIME, TAD, TAFD, ELTM")
    return(NULL)
  }
  # don't check POSIX column
  posix = time_cols[sapply(.data[time_cols], inherits, "character")]
  time_cols = setdiff(time_cols,posix)
  if(length(posix)) message(sprintf("Time column(s) [%s] are formatted as dates and will not checked.", paste(posix,collapse=", ")))
  .data = .data %>%
    gather_(key_col="time.type", value_col = "theTime", gather_cols=time_cols)

  grps =  c("time.type","EVID", tokenize(strata))

  result = .data %>% group_by_(.dots = grps) %>%
    summarise(range=special.range(theTime),
              nr.negative = paste(sum_(theTime<0)),
              tad.tolerance.val = paste(tad.tolerance),
              nr.exceed.tolerance = paste(sum_(theTime>tad.tolerance))
    ) %>%
    mutate_cond(condition = time.type != "TAD"
                , nr.exceed.tolerance = ""
                , tad.tolerance.val = "")

  if(all(result$nr.negative=="0")) message("All time variables are OK.")

  if(any(result$nr.negative!="0"&result$time.type=="TAD"))
    message("Warning: Time after last Dose (TAD) column contains at least one negative value. This may need to be corrected.")
  if(any(result$nr.negative!="0"&result$time.type=="TAFD"))
    message("Warning: Time After First Dose (TAFD) column contains at least one negative value. This may need to be corrected.")
  if(any(result$nr.negative!="0"&result$time.type=="ELTM"))
    message("Warning: Elapsed Time (ELTM) column contains at least one negative value. This should be corrected.")
  if(any(result$nr.negative!="0"&result$time.type=="TIME"))
    message("Warning: Time (TIME) column contains at least one negative value. This may need to be corrected.")

  list(tab=result)
}

#' Summarise DV by strata or cutpoint to look for outliers
#'
#' Summarises DV by strata, if provided, or by cut points of idv.
#'
#' @param .data data
#' @param strata A comma separated string or character vector of columns to stratify by
#' @param cut2.arguments Arguments to Hmisc::cut2, as a list
#' @param idv The column to cut by
#'
#' @return a dataframe
#' @importFrom dplyr group_by_ summarise filter
#' @importFrom Hmisc cut2 smean.sdl
#' @export

table_outlier_byCuts = function(.data
                                , strata=""
                                , cut2.arguments = list(g = 5)
                                , idv = ""
)
{
  .data = .data %>% filter(EVID==0)
  grps=NULL
  if(idv!="")
  {
    .data$idv = eval(as.name(idv), .data)
    .data$time.cuts = do.call("cut2", append(list(x=.data$idv), cut2.arguments))
    grps = "time.cuts"
  }
  if(!strata=="") grps = c(tokenize(strata),grps)

  result = .data %>% group_by_(.dots = grps) %>%
    summarise(n = length(DV),
              mean=mean(DV, na.rm = T),
              lmean=exp(mean(log(DV))),
              median=median(DV, na.rm = T),
              range = special.range(DV),
              ci95.lo = exp(smean.sdl(log(DV))[2]),
              ci95.hi = exp(smean.sdl(log(DV))[3])
    )
  if(idv!="") result=result %>% rename_(.dots=setNames("time.cuts",idv))

  list(tab=result)
}

#' Plot outliers by cutpoints
#'
#' @param .data data
#' @param strata strata
#' @param idv independent variable
#' @param cut2.arguments cut2 arguments
#'
#' @return a ggplot2 plot object
#' @importFrom ggplot2 ggplot geom_jitter aes position_jitter
#' @export
#'
plot_outlier_byCuts = function(.data
                               , strata=""
                               , idv = "TAD"
                               , cut2.arguments = list(g = 5)
)
{
  .data = .data %>% filter(EVID==0)
  grps=NULL

  if(idv!="")
  {
    .data$idv = eval(as.name(idv), .data)
    .data$time.cuts = do.call("cut2", append(list(x=.data$idv), cut2.arguments))
    grps = "time.cuts"
    p=ggplot(.data, aes(y=DV, x=time.cuts)) #+ theme_bw()
  }
  else{
    p=ggplot(.data, aes(y=DV, x=factor(0))) + #theme_bw() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }
  if(strata!="")
  { update_geom_defaults("point", list(colour = NULL))
    p=p+geom_boxplot(aes_string(color= sprintf("interaction(%s)", strata))) + labs(color="Strata") +
      facet_wrap(reformulate(tokenize(strata)),
                  scales = "free")
    update_geom_defaults("point", list(colour = "black"))
  }
  else {
    p=p+geom_boxplot()
  }
  p + theme_bw() +
    theme(axis.text.x=element_text(angle=90,hjust=1))
}

#' Plot NONMEM data as spaghetti plot
#'
#' The spaghetti plot will be formed using DV values, stratified by
#' variables in strat.  Add CMT to strat if there are multiple observations.
#' Data will be colored by combinations of variables found in grouping.
#'
#' @param .data NONMEM data as a data.frame
#' @param idv A comma delimited string of variables for the x axis
#' @param strat A comma delimited string of stratification variables.
#' @param grouping A comma delimited string of grouping variables.
#' @param ID a character value giving the column containing subject IDs
#'
#' @return a list of ggplot2 plot objects, one for each idv.
#' @importFrom ggplot2 ggplot geom_line geom_point facet_grid aes
#' @export
#'
plot_spaghetti = function(.data
                          , strata=""
                          , grouping=""
                          , idv="TIME"
                          , logY=F
                          , ID="ID"
)
{
  #.data = apply.map(.data, map = MAP) %>% filter(EVID!=2) # remove simulation points
  .data = .data %>% filter(EVID!=2)
  if(idv=="") idv="TIME" # reset default
  if(ID=="") ID="ID" # reset default

  spagplot = function(.idv, .df)
  {
    if(grouping!="")
    {
    p = ggplot(.df, aes_string(y="DV", x=.idv, group=ID,
                               color= sprintf("interaction(%s)", grouping)
                               )
               )
    }
    else
    {
      p = ggplot(.df, aes_string(y="DV", x=.idv, group=ID))
    }
    p=p+
      geom_line(data = subset(.df,EVID==0)) +
      geom_point(data = subset(.df,EVID==0),alpha = 0.33)
    if(logY) p=p+scale_y_log10()
    if(strata!=""){
      #p=p + facet_grid(sprintf("%s~.",paste(tokenize(strata),collapse = "+")), scales = "free")
      p=p + facet_wrap(reformulate(tokenize(strata)), ncol=1, scales = "free")}
    p + labs(color="Groups") + theme_bw()
  }
  lapply(tokenize(idv),spagplot,.data)
}

#' Plot individual subject profiles in NONMEM dataset
#'
#' The individual plot will be formed using DV values, stratified by
#' variables in strat.  Add CMT to strat if there are multiple observations.
#' Data will be colored by combinations of variables found in grouping.
#'
#' @param .data NONMEM data as a data.frame
#' @param idv A comma delimited string of variables for the x axis
#' @param strat A comma delimited string of stratification variables.
#' @param grouping A comma delimited string of grouping variables.
#' @param ID a character value giving the column containing subject IDs
#'
#' @return a list of ggplot2 plot objects, one for each idv.
#' @importFrom ggplot2 ggplot geom_line geom_point facet_grid aes
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line geom_point facet_grid aes
#' @export
#'

plot_individual = function(.data
                           , idv="TIME"
                           , strata=""
                           , grouping=""
                           , logY=F
                           , showADDL=F
                           , ID="ID"

)
{
  # remove simulation points and non-finite DV
  .data = .data %>% filter(EVID!=2)
  if(idv=="") idv="TIME" # reset default
  if(ID=="") ID="ID" # reset default

  indplot = function(.idv, .df)
  {
    if(grouping!="")
    {
      p = ggplot(.df, aes_string(y="DV", x=.idv,
                                 color= sprintf("interaction(%s)", grouping)
                                 )
      )
    }
    else
    {
      p = ggplot(.df, aes_string(y="DV", x=.idv))
    }
    p=p+
      geom_line(data = subset(.df,EVID==0)) +
      geom_point(data = subset(.df,EVID==0),alpha = 0.33)
    if(showADDL==T & all(c(ID,"ADDL","II") %in% names(.df))){
      p = p + geom_vline(data=.df %>% filter((EVID==1 | EVID==4) & ADDL!=-1) %>% select_(TIME=.idv,"ADDL","II",ID) %>% expand_addl(ii_scale=1),
                         aes(xintercept = TIME,
                             linetype = factor(ifelse(ADDL == -1, 2, ifelse(ADDL ==0, 4, 3))))
                             ) +
        scale_linetype_discrete(
          name = "Record Type",
          labels = c("Original", "Derived", "Expanded"))

    }
    if(logY) p=p+scale_y_log10()
    if(strata!="")
    {  p=p + facet_grid(sprintf("%s~%s",paste(tokenize(strata),collapse = "+"),
                                ID),
                        scales = "free")}
    else
    {
      p=p+facet_grid(as.formula(paste(".~", ID)), scales="free")
    }
    p + labs(color="Groups") + theme_bw()
  }
  lapply(tokenize(idv),indplot,.data)
}


#' Find subjects with no dosing records
#'
#' @param .data a data.frame or tbl object
#' @param ID a character value giving the column containing subject IDs
#'
#' @return a single column tbl with unique IDs for subjects with no dosing records
#' @importFrom dplyr group_by summarise filter select
#' @export
#'

find_subjects_no_dosing = function(.data, ID="ID")
{
  .data %>%
    group_by_(.dots=ID) %>%
    summarise(n=sum(EVID==1)) %>%
    filter(n==0) %>%
    select_(.dots=ID)
}


#' Find subjects with no observation records
#'
#' @param .data a data.frame or tbl object
#' @param ID a character value giving the column containing subject IDs
#'
#' @return a single column tbl with unique IDs for subjects with no observations
#' @importFrom dplyr group_by summarise filter select
#' @export
#'
find_subjects_no_obs = function(.data, ID="ID")
{
  .data %>%
    group_by_(.dots=ID) %>%
    summarise(n=sum(EVID==0)) %>%
    filter(n==0) %>%
    select_(.dots=ID)
}

#' Find placebo subjects with positive DV
#'
#' The data is filtered according to is.placebo & condition, along with EVID==0 (to only query observations) and the
#' unique subject IDs satisfying all conditions are returned.  Condition may be a compound test, such as
#' "DV>0 & CMT==2".
#'
#' @param .data a data.frame or tbl object
#' @param is.placebo An R expression, as character, that is used to determine if a subject is a placebo subject.
#' @param condition An R expression, as character, that is tested for the placebo subjects.
#' @param ID A character value giving the column containing subject IDs
#'
#' @return a single column tbl with unique IDs for placebo subjects meeting the criteria
#' @importFrom dplyr group_by summarise filter select
#' @export
#'
find_subjects_placebo_obs = function(.data, is.placebo, condition="DV>0", ID="ID")
{
  # need to identify placebo subjects in argument is.placebo
  if(missing(is.placebo)) stop("Placebo condition (e.g. TRTA=='placebo') must be supplied")
  # the arguments come in as strings, so capture them as lazy expressions in caller frame
    # need to do this because filter_ doesn't capture correct environment for the expressions as text
  is.placebo = lazyeval::as.lazy(is.placebo,env=parent.frame())
  condition = lazyeval::as.lazy(condition,env=parent.frame())
  # then wrap mutate_cond and return just the obs we find
  .data %>%
    filter_(is.placebo, condition, "EVID==0") %>%
    dplyr::distinct_(.dots=ID)
}


## descriptive summary

#' Summary table of dataset
#'
#' @param .data NONMEM dataset
#' @param cols character vector containing columns to summarise
#' @param grouping character vector containing columns to group by
#' @param catFn function to use when summarising non-numeric cols
#' @param conFn function to use when summarising numeric cols
#'
#' @importFrom dplyr group_by_ data_frame select_ summarise_each left_join
#' @return a dataframe
#' @export
#'

summary_table = function(.data, cols="", grouping="", catFn=table, conFn=summary){

  cols = tokenize(cols)
  grps = tokenize(grouping)

  if(grouping!="") .data = .data %>% group_by_(.dots=grps)

  if(length(cols)==0) stop("Must provide column names for summary")
  catCols = cols[cols %in% names(.data)[!sapply(.data,is.numeric)]]
  conCols = cols[cols %in% names(.data)[sapply(.data,is.numeric)]]
  # start with empty summary over groups.  XX__ is to get a row per group (or only one row if no groups)
  # THen get unique groupID, then remove XX so it's not a nuisance later
  catsum = .data %>% summarise(XX__=1) %>% mutate(GRPID__=1:n()) %>% select(-XX__)
  consum = catsum
  if(length(catCols)>0) catsum = .data %>% select_(.dots=catCols) %>%
    summarise_each(funs(format_summary(.,catFn))) %>%
    mutate(GRPID__=1:dplyr::n())
# start with summary over groups
  if(length(conCols)>0) consum = .data %>% select_(.dots=conCols) %>%
    summarise_each(funs(format_summary(.,conFn))) %>%
    mutate(GRPID__=1:dplyr::n())
  # now join by groupID.  in case of no groups groupID is just 1 and there is one row
  left_join(catsum, consum, by="GRPID__") %>% select(-GRPID__)
}

# internal function to summarise a column according to passed in function.  If sumFn returns a vector, the elements
# are stacked in the resulting string
format_summary = function(x, sumFn=summary, ...){
  # ... additional parameters for sumFn
  sumry=sumFn(x,...)
  paste(sprintf("%s   %s\\\n",names(sumry),sumry), collapse = "")
}

## Summary plots

#' Summary plot of dataset
#'
#' @param .data NONMEM dataset
#' @param cols character vector containing columns to summarise
#' @param grouping character vector containing columns to group by
#'
#' @importFrom dplyr group_by_ data_frame select_
#' @importFrom tidyr gather_
#' @importFrom ggplot2 aes_string geom_bar facet_wrap labs theme_bw geom_histogram
#' @return a dataframe
#' @export
#'

covariate_plots = function(.data, cols="", grouping=""){

  cols = tokenize(cols)
  grps = tokenize(grouping)

  #if(grouping!="") .data = .data %>% group_by_(.dots=grps)

  if(length(cols)==0) stop("Must provide column names for plot")
  catCols = cols[cols %in% names(.data)[!sapply(.data,is.numeric)]]
  conCols = cols[cols %in% names(.data)[sapply(.data,is.numeric)]]
  catsum = data_frame()
  plots=list()
  if(length(catCols)>0)
  { plots$pcat = ggplot(.data %>% select_(.dots=c(tokenize(c(catCols, grouping)))) %>%
                    gather_(key_col = "Covariate", value_col = "Value", gather_cols = catCols)) +
      facet_wrap(~Covariate,ncol=4, scales="free") + theme_bw() +
      theme(axis.text.x=element_text(angle=90,hjust=1))
    if(grouping!="")
      plots$pcat = plots$pcat + geom_bar(aes_string(x="Value", fill=sprintf("interaction(%s)", grouping)),
                                         stat="bin",position="dodge") + labs(fill="Groups")
    else
      plots$pcat = plots$pcat + geom_bar(aes_string(x="Value"), stat="bin",position="dodge")

  }
  if(length(conCols)>0)
  { plots$pcon = ggplot(.data %>% select_(.dots=c(tokenize(c(conCols, grouping)))) %>%
                    gather_(key_col = "Covariate", value_col = "Value", gather_cols = conCols)) +
                      facet_wrap(~Covariate,ncol=4, scales="free") + theme_bw()
    if(grouping!="")
      plots$pcon = plots$pcon +
        geom_histogram(aes_string(x="Value", fill=sprintf("interaction(%s)", grouping)),position="stack") +
        labs(fill="Groups")
    else
      plots$pcon = plots$pcon +
        geom_histogram(aes_string(x="Value"),position="stack")
  }
  plots
}

# internal function to summarise a column according to passed in function.  If sumFn returns a vector, the elements
# are stacked in the resulting string
format_summary = function(x, sumFn=summary, ...){
  # ... additional parameters for sumFn
  sumry=sumFn(x,...)
  paste(sprintf("%s   %s\\\n",names(sumry),sumry), collapse = "")
}
