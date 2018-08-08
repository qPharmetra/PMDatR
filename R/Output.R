#' Format a dataframe for output to nonmem
#'
#' @param df A tbl object
#' @param base_name The filename of the output csv
#' @param opts A list with options for digits and na
#'
#' @return A tbl object, df formatted for output to nonmem.
#' @export
#'

OutputDataset = function(df, base_name, opts=NULL, column_info=NULL){
  # format df columns and save NONMEM data file

  if(missing(opts)){
    # set defaults if opts is missing
    opts$digits = coalesce(opts$digits, options("digits"))
    opts$na = coalesce(opts$na,".")
  }

  # order the columns by column_info, rename to OutputName, and drop requested columns
  if(!is.null(column_info)){
    cnames = purrr::map_chr(column_info,"Name")
    onames = purrr::map_chr(column_info,"OutputName", .null=NA)
    include = purrr::map_lgl(column_info,"Include", .null=F)
    #priority = purrr::map_int(column_info,"Priority", .null=NA)
    priority=integer()
    for(i in 1:length(column_info)){
      if(is.null(column_info[[i]]$Priority)){
        priority[i] = NA
      } else{
        priority[i] = column_info[[i]]$Priority
      }
    }
    if(length(onames)>0) {
      # we have output names so use them, if any were missing replace with the name
      idx.na = is_one_of(onames,"",NA)
      onames[idx.na] = cnames[idx.na] #if oname is NA, just keep the original name
      names(cnames)=onames
    }

    # set default pnames to cnames
    pnames=cnames
    if(!all(is.na(priority))){
      #get sort order of priorities
      psort = sort(priority)
      # get indices of sorted priorities
      pmatch = match(psort,priority)
      # get the ordered list of names... drops not processed yet
      pnames = c(cnames[pmatch],cnames[is.na(priority)])
    }

    # drop columns that are not included
    if(length(include)>0) cnames = cnames[include]

    # don't select columns that are not in the data
    not.here = setdiff(cnames, names(df))
    if(length(not.here)>0){
      warning(sprintf("RT3: - Requested output columns are not in the dataset.  Check output mappings for: %s",
                      paste(not.here, collapse=", ")))
      cnames=cnames[cnames %in% names(df)]

    }
    # by this point cnames has the list of valid names, pnames has the order
    cnames = pnames[pnames %in% cnames]
    # this will select by the value in cnames, but rename to the name in cname
    df = df %>% ungroup() %>% select_(.dots=cnames)
  }

  # write database file
  write.csv(df, paste0(base_name,".database.csv"), quote=T, row.names = F)

  # write NONMEM file
  out.df=formatForNONMEM(df, column_info)
  write.csv(out.df, paste0(base_name,".csv"), quote=F, row.names = F, na=opts$na)

  out.df
}


formatForNONMEM = function(df, column_info=NULL){

  formatColumn = function(x){
    # format based on column data type
    y=x
    ctype = attr(x,"Type")
    if(ctype=="Numeric"){
      # do we need y=as.numeric(y) here?
      y=as.numeric(x)
      colFormat = attributes(x)$Format
      if(!is_valid_format_spec(colFormat)) colFormat="%.4g"  # should accept user default for this
      y = ifelse(is.na(y),NA,sprintf(colFormat,y))
      attributes(y) = attributes(x)
    }
    if(ctype=="Categorical"){
      fac = factor(x)
      y = as.integer(fac)
      attributes(y) = c(attributes(x), list(levels=levels(fac)))
    }
    if(ctype=="Logical"){
      y = as.integer(x)
      attributes(y) = c(attributes(x), list(levels=c("FALSE","TRUE")))
    }
    if(ctype=="DateTime"){
      y = as.character(x) #as.POSIXct(x, origin="1970-01-01", tz="GMT"))
      attributes(y) = attributes(x)
      class(y) = "character" # otherwise print.posix will cause NA values
    }
    if(ctype=="Text"){
      y=as.character(x)
      attributes(y) = attributes(x)
    }
    y
  }

  default_column_attributes = function(col){
    if(inherits(col,c("integer","numeric", "difftime"))){
      al = list(Type="Numeric", Format="%.4g", Units="")
    }
    if(inherits(col,c("character","factor"))){
      if(length(unique(col))>10){
        al = list(Type="Text", Format="%s", Units="")
      } else {
        al = list(Type="Categorical", Format="%s", Units="")
      }
    }
    if(inherits(col,"logical")){
      al = list(Type="Logical", Format="%s", Units="")
    }
    if(inherits(col,c("POSIXct"))){
      al = list(Type="DateTime", Format="%s", Units="")
    }
    al
  }

  # column_info is a list by column name with Type, Format, and Units
  if(!is.null(column_info)){
    # df names are output names, need to still reference column_info by internal name
    inames = setNames(purrr::map_chr(column_info, "Name"), purrr::map_chr(column_info, "OutputName"))
    for(cname in names(df)){
      al = column_info[[inames[cname]]]
      # didn't find anything in the column_info for this column so set defaults
      if(is.null(al)){
        al=default_column_attributes(df[[cname]])
      }

      attributes(df[[cname]]) = c(attributes(df[[cname]]),al) # preserve existing attributes, like pmunits class
    }
  } else {
    # no column_info so just set defaults
    for(cname in names(df)){
      al=default_column_attributes(df[[cname]])
      attributes(df[[cname]]) = c(attributes(df[[cname]]),al) # preserve existing attributes, like pmunits class
    }
  }

  df= df %>% ungroup() %>%  mutate_each(funs(formatColumn(.)))

  # strip levels from ID, this is a kluge until we can carry type info through the merge
  attributes(df$ID)$levels<-NULL
  df
}

#' Generate Data Definition Table
#'
#' @param DM A data management object
#' @param .data A NONMEM database (not yet formatted for NONMEM)
#'
#' @return A tbl object with columns for Column name, Type, Codes, Mapping, Source
#' @importFrom dplyr bind_rows left_join as_data_frame
#' @importFrom stringr str_count str_dup
#' @export

createDDD = function(DM, .data){

  maps = list()
  # We want for each output column: Name, Type, Codes, Source, Mapping, Transform

  # iterate queries in DM getting column mappings
  # when we look in queries, figure out if a mapping is from stacked Parameter/Values
  for(qry in c(DM$DVs, DM$IndDoses, DM$Covs, DM$CovTs)){
    maps = c(maps, get_mappings.Query(qry$mappings,
                                      DM$Settings$DomainMetaData[[qry$mappings$Domain]],
                                      DM$Domains[[qry$mappings$Domain]]$InputMappings$Columns))
  }
  maps=do.call(rbind,lapply(maps, dplyr::as_data_frame))
  # Get column information from the attributes of the data columns
  lapply(1:ncol(.data), function(x) list(ColumnName=names(.data)[x],
                                      Type=attributes(.data[[x]])$Type,
                                      Codes=paste(sprintf("%i: %s", 1:length(levels(.data[[x]])), levels(.data[[x]])),
                                                  collapse="\\\n"),
                                      Units=attributes(.data[[x]])$pmunits )) -> ddd

  # bind list into a dataframe
  ddd=lapply(ddd,function(x){if(is.null(x$Units))x$Units="";x})
  ddd=do.call(bind_rows,ddd) #get a tbl

  # get Name, OutputName, Fill from OutputCOlumns
  purrr::map(DM$Settings$OutputColumns, function(x) list(Column=x$Name,
                                         Transform=x$Fill,
                                         ColumnName=x$OutputName,
                                         Dropped=!x$Include))-> ooo
  # bind list into a data frame
  ooo=do.call(bind_rows,unname(ooo)) #get a tbl

  # join ooo and mapping info, keep ooo and drop unmatched mappings (can't think how this would happen)
  ooo=left_join(ooo,maps, by="Column")
  # join ddd and OutputColumns info, keeping just the info in the actual data (this drops DROPPED columns)
  ddd=left_join(ddd,ooo, by="ColumnName")
  # bindrows any dropped columns
  ddd = bind_rows(ddd,ooo %>% filter(Dropped) %>% mutate(ColumnName=sprintf("%s[dropped]",Column)))

  # select to reorder
  ddd %>% select(Column=ColumnName, InternalName=Column, Type, Codes, Units, Domain, Condition, Source_Columns, Sources, Mapping, Transform)
}

#' Format a DDD object as a dataframe
#'
#' @param ddd A DDD data frame
#' @param type md for markddown formatting of multiline cells
#'
#' @return a data_frame (tbl_df) object
#' @export

formatDDD = function(ddd, type="md"){
  # print columns with Name, Type, Range (min -- max, or multiline uniques)
  #ensure we maintain the existing row order by joining to existing Column column
  full_join(data_frame(Column=unique(ddd$Column)),
            ddd %>% group_by(Column) %>% summarise_each(funs(slash.reps(.))),
            by="Column")

}

slash.reps = function(x) {
  # given a vector with repeated values, collapse it and replace repeated values with " \\\n"
  n=length(x)
  if(n>1){
    y=c(T,x[2:n]!=x[1:(n-1)])
    x[!y]=" "
    x = paste0(x,collapse="\\\n")
  }
  x

}

#' Get column attributes for nonmem formatted output
#'
#' @param .data a dataframe
#'
#' @return a dataframe
#' @export
#'

get_nm_attributes = function(.data){
  # Get column information from the attributes of the data columns
  lapply(1:ncol(.data), function(x) list(ColumnName=names(.data)[x],
                                         Type=attributes(.data[[x]])$Type,
                                         Codes=paste(sprintf("%i: %s", 1:length(levels(.data[[x]])), levels(.data[[x]])),
                                                     collapse="\\\n"),
                                         Units=attributes(.data[[x]])$pmunits )) -> ddd

  # bind list into a dataframe
  ddd=lapply(ddd,function(x){if(is.null(x$Units))x$Units="";x})
  ddd=do.call(bind_rows,ddd) #get a tbl
  ddd
}
