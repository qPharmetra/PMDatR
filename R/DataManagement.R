

#' Create a Data Management Object
#'
#' @param settings_file The YAML settings file to use
#' @param settings A list of settings to use instead of a file
#'
#' @return A DataManagement object
#' @importFrom yaml yaml.load_file
#' @importFrom purrr map_chr
#' @export
#'

DataManagement <- function(settings_file="", settings=NULL){

  if(is.null(settings)){
    # load the settings file
    settings = load_yaml_file(settings_file)
  }

  # flag errors
  all_valid = T
  #setup error list, and function for easy appending to it
  errs = list()
  append.err = function(err){
    errs<<-c(errs,err)
  }

  # process header, do this first because we need to source the header code
  if(!is.empty.yaml(settings$Header)){
    header = settings$Header
    # Premerge hook is function body.  it must all parse as valid R expression
    tryCatch({
      parse(text=header)},
    error=function(e){
      append.err(sprintf("Error DM6: 'Header' contains an invalid R expression:\n%s", e$message))
      }
    )
    # source into global environment so error checking works below
    tryCatch({
      tmp = tempfile()
      writeLines(header,tmp)
      sys.source(tmp, globalenv())
    },
    error=function(e){
      append.err(sprintf("Error DM13: 'Header' processed with error: \n%s", e$message))
    },
    finally={
      unlink(tmp)
    }
    )
  }

  # set names in the DomainMetaData
  if(!is.empty.yaml(settings$DomainMetaData)){
    #metadata could have nulls if load failed.  Domains come in in same order and must have names
    names(settings$DomainMetaData) = map_chr(settings$Domains,"name")
    for(i in seq_along(settings$DomainMetaData)){
      # safety check for null in columns
      if(!is.null(settings$DomainMetaData[[i]]$Columns))
        {names(settings$DomainMetaData[[i]]$Columns) = map_chr(settings$DomainMetaData[[i]]$Columns, "name")}
    }}

  # process domains
  Domains = list()
  for(dom in settings$Domains){
    Domains[[dom$name]] = domain(dom)
    all_valid = all_valid & Domains[[dom$name]]$valid
  }

  # process queries

  DVs = list()
  for(qry in settings$DependentVariables){
    DVs[[qry$Name]] = Query_DV(qry, settings$DomainMetaData[[qry$Domain]])
    all_valid = all_valid & DVs[[qry$Name]]$valid
    }
  Covs = list()
  for(qry in settings$Covariates){
    Covs[[qry$Name]] = Query_Cov(qry, settings$DomainMetaData[[qry$Domain]])
    all_valid = all_valid &  Covs[[qry$Name]]$valid
    }
  CovTs = list()
  for(qry in settings$TVCovariates){
    CovTs[[qry$Name]] = Query_CovT(qry, settings$DomainMetaData[[qry$Domain]])
    all_valid = all_valid &  CovTs[[qry$Name]]$valid
    }

  IndDoses = list()
  for(qry in settings$Doses){
    IndDoses[[qry$Name]] = Query_IndDose(qry, settings$DomainMetaData[[qry$Domain]] )
    all_valid = all_valid &  IndDoses[[qry$Name]]$valid
  }

  # process pre-merge hook
  if(!is.empty.yaml(settings$PreMergeHook)){
    pre_hook = settings$PreMergeHook
    # Premerge hook is function body.  it must all parse as valid R expression
    tryCatch(parse(text=pre_hook), error=function(e){append.err(sprintf("Error DM1: 'PreMergeHook' is not a valid R expression", e$message))})
  }
  # process post-merge hook
  if(!is.empty.yaml(settings$PostMergeHook)){
    post_hook = settings$PostMergeHook
    # Premerge hook is function body.  it must all parse as valid R expression
    tryCatch(parse(text=post_hook), error=function(e){append.err(sprintf("Error DM2: 'PostMergeHook' is not a valid R expression", e$message))})
  }

  # check OutputColumns Fill and Transform
  # if outputcolumns element is present
  if(!is.null(settings$OutputColumns)){
    validnames = Filter(function(x) x$Type !="PostTransform", settings$OutputColumns)
    validnames = Filter(function(x) is_valid_variable_name(x$Name), validnames)
    validnames = purrr::map_chr(validnames,"Name")
    names(settings$OutputColumns) = sapply(settings$OutputColumns, function(x)x$Name)
    for(col in settings$OutputColumns){
      if(!is_valid_variable_name(col$Name)) append.err(sprintf("Error DM3: Output column name '%s' is not valid.", col$Name))
      if(!is.empty.yaml(col$Fill)){
        #tryCatch(parse(text=col$Fill),
        #         error=function(e) {append.err(sprintf("Error DM4: Output column '%s' Fill/CFB setting is not a valid R expression. %s", col$Name, e$message))}
        #         )
        # check the mapping
        checks = check_mapping(col$Fill, validnames)
        if(checks$parse_err) {append.err(sprintf("Error DM4: Output column '%s' Fill/CFB setting '%s' is not a valid R expression.\n%s",
                                                 col$Name, col$Fill, checks$expr$message))}
        if(checks$name_err) {append.err(sprintf("Error DM5: Output column '%s' Fill/CFB setting : \n %s \n names [%s] not found in Output Columns List",
                                                col$Name, col$Fill, paste(checks$bad_names,collapse=", ")))}
        ## after checking, add the PostTransform column name to valid names,
        #    to allow subsequent columns that reference it to pass checks
        if(col$Type=="PostTransform") validnames=c(validnames,col$Name)
      }
    }
  }

  ## check unique names in domains and queries
  unames = c(map_chr(settings$Domains, "name"),
             map_chr(c(settings$DependentVariables, settings$Doses,
                       settings$TVCovariates, settings$Covariates),
                     "Name"))
  if(any(duplicated(unames))){
    append.err(sprintf("Error DM15: Source and Query names [%s] are not unique.",
                       paste(unames[duplicated(unames)], collapse=", ")))
  }

  ## Check MergeOptions
  merge_options=NULL
  if(!is.null(settings$Options)){
    merge_options=settings$Options
    #check nPrintRows
    if(!is.empty.yaml(merge_options$nPrintRows)){
      if(merge_options$nPrintRows<0) append.err("Error DM7: nPrintRows must be greater than 0.")
    }
    # check EVID Order
    if(!is.empty.yaml(merge_options$EVIDorder)){
      # convert to numeric vector, check for NA (non-numeric) and check that ALL EVIDS given
      merge_options$EVIDorder = suppressWarnings(as.numeric(tokenize(merge_options$EVIDorder)))
      if(any(is.na(merge_options$EVIDorder)) | !all(0:4 %in% merge_options$EVIDorder)){
        append.err(sprintf("Error DM8: EVID Order must contain comma seperated values 0 through 4. '%s' is not valid",
                   settings$Options$EVIDorder))
      }
    }
    # check ExpandADDL
    if(!is.empty.yaml(merge_options$ExpandADDL)){
      if(!is.logical(merge_options$ExpandADDL)) append.err("Error DM9: ExpandADDL must be TRUE or FALSE.")
    }
    # check ADDLTolerance
    if(!is.empty.yaml(merge_options$ADDLTolerance)){
      if(!is.numeric(merge_options$ADDLTolerance)){
        append.err("Error DM12: ADDLTolerance must be numeric between 0 and 1.")
      } else if(merge_options$ADDLTolerance<0 | merge_options$ADDLTolerance>1.0 ){
        append.err("Error DM12: ADDLTolerance must be numeric between 0 and 1.")
      }
    }
    # check SaveIntermediateFiles
    if(!is.empty.yaml(merge_options$SaveIntermediateFiles)){
      if(!is.logical(merge_options$SaveIntermediateFiles)){
        append.err("Error DM16: SaveIntermediateFiles must be TRUE or FALSE")
      }
    }
    # check KeepEVID2
    if(!is.empty.yaml(merge_options$KeepEvid2)){
      if(!is.logical(merge_options$KeepEvid2)) append.err("Error DM10: KeepEVID2 must be TRUE or FALSE.")
    }
    # check SortOrder
    if(!is.empty.yaml(merge_options$SortOrder)){
      # convert to character array, and check that all elements are in OutputColumns
      merge_options$SortOrder = tokenize(merge_options$SortOrder)
      badcols=character() #initialze to zero length
      if(!is.empty.yaml(settings$OutputColumns))
        # remove - from SortOrder when comparing with column names
        badcols = setdiff(gsub("^-", "", merge_options$SortOrder),names(settings$OutputColumns))
      if(length(badcols)>0)
        append.err(sprintf("Error DM11: SortOrder must contain valid Output Column names. Bad names: [%s]",
                   paste(badcols, collapse=", ")))
    } else{
      #if SortOrder is empty, ensure it is null
      merge_options$SortOrder=NULL
    }
    # check ADDLGrouping
    if(!is.empty.yaml(merge_options$ADDLGrouping)){
      # convert to character array, and check that all elements are in OutputColumns
      merge_options$ADDLGrouping = tokenize(merge_options$ADDLGrouping)
      badcols=character() #initialze to zero length
      if(!is.empty.yaml(settings$OutputColumns))
        # remove - from SortOrder when comparing with column names
        badcols = setdiff(gsub("^-", "", merge_options$ADDLGrouping),names(settings$OutputColumns))
      if(length(badcols)>0)
        append.err(sprintf("Error DM14: ADDLGrouping must contain valid Output Column names. Bad names: [%s]",
                           paste(badcols, collapse=", ")))
    } else{
      #if ADDLGrouping is empty, ensure it is null
      merge_options$ADDLGrouping=NULL
    }

  }


  all_valid = all_valid & length(errs)==0
  # return DataManagement object
  structure(list(Settings=settings, Domains=Domains, DVs=DVs, Covs=Covs, CovTs=CovTs,
                 IndDoses=IndDoses, MergeOptions = merge_options,
                 errors = errs, valid=all_valid),
            class="DataManagement")
}


#' Write R functions corresponding to a DataManagement objet
#'
#' @param DMobj A data management object
#'
#' @return Text containing the functions that can be sourced or saved to a file.
#' @export
#' @importFrom formatR tidy_source
#'

WriteCode.DataManagement <- function(DMobj)
{
  if(!DMobj$valid) stop("Errors in ")
  #txt is the character string with the code
  txt=""
  #local utility function
  addLine <- function(addtxt) {txt<<-paste(txt, addtxt, sep="\n")}

  #write header (library statements, etc)
  if(!is.empty.yaml(DMobj$Settings$Header)){
    addLine(sprintf("### Header\n%s\n",DMobj$Settings$Header))
  }

  #write loading of domains
  addLine("### Load and Preprocess domains")
  addLine("preprocess.domains <- function(DMobj){")
  for(dom in DMobj$Domains){
    addLine(sprintf("DMobj$Domains$%s = load.domain(DMobj$Domains$%s, .fun=preprocess_%s, .hook=preprocessHook_%s)",
                    dom$name, dom$name, dom$name, dom$name))
    addLine("# put the data into a global variable named after the domain\n")
    addLine(sprintf("%s <<- DMobj$Domains$%s$Data", dom$name, dom$name))
  }
  addLine("DMobj}")

  # write preprocess domain functions
  addLine("\n## Preprocess hook functions allow for free form modification of loaded data.
  ##  Modify the data object and return it.\n")
  addLine("\n## Mapping functions apply filters, transformations, pre-merge specified by settings.
  ##  Set data in dom$Data, and return dom.\n")

  for(dom in DMobj$Domains){
    addLine(sprintf("# pre-processing hook for domain: %s\n", dom$name))
    addLine(dom$fnHook)
    addLine("\n")
    addLine(sprintf("# Mapping function for domain: %s\n", dom$name))
    addLine(dom$fnPreProc)
    addLine("\n")
  }

  # Process Dosing
  addLine("Process_Dose <- function(){")
  qnams=character()
  for(qry in DMobj$IndDoses){
    addLine(sprintf("%s = %s", qry$mappings$Name, qry$fnCall))
    qnams=c(qnams,qry$mappings$Name)
  }
  if(length(qnams)) addLine(sprintf("bind_rows(%s)\n", paste(qnams, collapse=",")))
  addLine("}\n")

  #Process DVs
  addLine("Process_DV <- function(){")
  qnams=character()
  for(qry in DMobj$DVs){
    addLine(sprintf("%s = %s", qry$mappings$Name, qry$fnCall))
    qnams=c(qnams,qry$mappings$Name)
  }
  if(length(qnams)) {addLine(sprintf("bind_rows(%s)\n", paste(qnams, collapse=",")))}
  addLine("}\n")

  #Process Covs
  addLine("Process_Cov <- function(){")
  qnams=character()
  for(qry in DMobj$Covs){
    addLine(sprintf("%s = %s", qry$mappings$Name, qry$fnCall))
    qnams=c(qnams,qry$mappings$Name)
  }
  if(length(qnams)){
    addLine(sprintf("list(%s)\n", paste(sprintf("%s=%s",qnams,qnams), collapse=",")))
  }
  addLine("}\n")

  #Process CovTs
  addLine("Process_CovT <- function(){")
  qnams=character()
  for(qry in DMobj$CovTs){
    addLine(sprintf("%s = %s", qry$mappings$Name, qry$fnCall))
    qnams=c(qnams,qry$mappings$Name)
  }
  if(length(qnams)) {addLine(sprintf("bind_rows(%s)\n", paste(qnams, collapse=",")))}
  addLine("}\n")

  ## pre-merge hook
  addLine("# pre-merge hook function\n")
  pre_hook = ""
  if(!is.empty.yaml(DMobj$Settings$PreMergeHook)){
    pre_hook = sprintf("\n%s\n",DMobj$Settings$PreMergeHook)
  }
  addLine(sprintf("pre.merge.hook = function(){%s}", pre_hook))
  addLine("\n")

  ## post-merge hook
  addLine("# post-merge hook function\n")
  post_hook = ""
  if(!is.empty.yaml(DMobj$Settings$PostMergeHook)){
    post_hook = sprintf("\n%s\n",DMobj$Settings$PostMergeHook)
  }
  addLine(sprintf("post.merge.hook = function(){%s}", post_hook))
  addLine("\n")

  ## post merge transform
  ## These functions will work with the merged dataset, passed in as .data.  Return the modified dataset.
  addLine("## These functions will work with the merged dataset, passed in as .data.  Return the modified dataset.\n")
  addLine(sprintf("post.transform = %s", write_post_transform(DMobj)))
  addLine("\n")
  addLine("post.filter = function(.data){.data}")
  addLine("\n")
  addLine("apply.exclusions = function(.data){.data}")

  tidy_source(text=txt, width.cutoff=50, output=F)
}

write_post_transform = function(DMobj){
  funcs=character()
  units=character()
  #iterate over all the queries and look for Fill settings in Column mappings
  for(col in DMobj$Settings$OutputColumns){
    if(!is.empty.yaml(col$Fill)){
      #can't fill internal type columns
      if(!is.empty.yaml(col$MappingType)){
        if(col$MappingType!="Internal") {
          funcs = c(funcs,
                    sprintf("post_transform_start('%s')", col$Name),
                    col$Fill,
                    sprintf("post_transform_end('%s')", col$Name)
                    )
        }
      }
    }
    # build unit conversion
    if(!is.empty.yaml(col$Units)){
      # there are units here, get them.
      if(grepl("MW",col$Units)){
        # If MW is provided in Units, be sure to set it up as a vector
        toks = tokenize(col$Units)
        units=c(units,sprintf("%s=c('%s', %s)",col$Name, toks[1], toks[2]))
      } else {
        units=c(units,sprintf("%s='%s'", col$Name, col$Units))
      }
    }
  }
  if(length(funcs)>0) {
    txt = paste(".data", paste(funcs, collapse=" %>%\n"), sep=" %>%\n")
    } else {
      txt=".data"
    }
  #if there are units in mapped columns
  if(length(units)>0) sprintf("%s %%>%%\n#assign units\nconvert_units_from_list(.ul=list(%s))\n",
                              txt, paste0(units, collapse=",")) ->txt
  sprintf("function(.data){\n%s\n}", txt)
}

# get a vector of column names from meta_data
get_columns_metadata = function(meta) {
  if(is.null(meta$Source)) return(NULL) #not valid meta_data
  unlist(purrr::map(meta$Columns,"name"))
  }
# get a vector of unique categorical values from meta_data
get_column_values_metadata = function(col) {
  if(is.null(col)) return(NULL)
  if(col$storage=="Categorical"){unlist(col$unique)} else NULL
}


#' @name post_transform_message
#' @aliases post_transform_start
#' @aliases post_transform_end
#' @title Send a message during a piped operation
#'
#' @param df a dataframe
#' @param msg A character value to print as a message.
#' @param colname  the column name being processed.  This is not checked for validity.
#'
#' @return the unchanged df dataframe
#'
#' @rdname post_transform_message
#' @export
post_transform_message= function(df, msg){
  message(msg)
  df
}

#' @rdname post_transform_message
#' @export
post_transform_start = function(df, colname){
  message(sprintf("Beginning post-transform processing of column [%s]", colname))
  df
}

#' @rdname post_transform_message
#' @export
post_transform_end = function(df, colname){
  message(sprintf("Finished post-transform processing of column [%s]\n", colname))
  df
}


