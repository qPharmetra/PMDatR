
#want domain object that represents: 1) the raw data 2) settings 3) processed imported data

#' Create a domain object from a list of settings.
#'
#' @details Settings may come from a yaml settings file, but must
#'   be converted to a list before passing in.
#'
#' @param dom.settings The settings
#'
#' @return A S3 object of class 'domain'
#' @export
#'
#'
domain = function(dom.settings){


  #dom.settings is a list full of settings info.  passed this way as settings structure may be mutable
  dom=dom.settings #shorten name to better work with it here

  # we can validate here, but if the file hasn't loaded we won't have meta_data yet, so semantic checks are skipped
  dom.settings$errors = validate.domain(dom)

  valid=F
  if(length(dom.settings$errors)==0) valid=T
  if(valid){
    dom.settings$fnPreProc = write_function.Domain(dom.settings)
    dom.settings$fnHook = write_hook.Domain(dom.settings)
  }
  dom.settings$valid=valid

  # check for changed mappings
  if(!is.null(dom.settings$DefaultMappings)){
    dom.settings$ChangedMappings = compare_mappings(dom.settings$DefaultMappings,
                                                    dom.settings$InputMappings)
  }


  #set empty metadata
  dom.settings$inMeta=reformat_metadata(dom$SourceMetaData$Columns) # keep incoming metadata
  dom.settings$outMeta=list()
  structure(dom.settings,class="domain")
}

#### validation function for domain object
validate.domain = function(dom){
  #helper function to determine if mapping is all whitespace
  is.blank = function(txt){grepl("^[[:space:]]*$", txt)}

  #setup error list, and function for easy appending to it
  errs = list()
  append.err = function(err){
    errs <<-c(errs,err)
  }

  # get valid column names from SourceMetaData, if present.  Note: first time load won't have it!
  #validnames=names(dom$Data) # is null if no data attached here
  validnames=NULL
  if(!is.null(dom$SourceMetaData)){
    validnames = get_columns_metadata(dom$SourceMetaData) # add '.' for some chaining functions to use
  }

  #check for required elements

  ## Name
  if(is.null(dom$name)){
    append.err("Error D1: Attempt to create Domain with no name")
    dom$name="unnamed"
  }
  else{
    # Name must be a legal R variable name
    if(!is_valid_variable_name(dom$name)) append.err(sprintf("Error D2: Domain name '%s' is not a valid R variable name.", dom$name))
  }

  #check for domain label
  #domname = dom.settings$name
  #if(is.null(domname)) stop("Malformed settings object used to create domain. Domain name must be present.")

  #check file path is present and exists

  if(is.null(dom$filepath))
  {
    append.err(sprintf("Error D3: Domain '%s' file path was not specified.", dom$name))
  } else{
    #check file path exists
    dom$filepath = printPath(dom$filepath)
    if(!file.exists(dom$filepath)) append.err(sprintf("Error D4: Domain '%s' File does not exist: %s",
                                                      dom$name, dom$filepath))
  }

  #check InputMappings

  inp = dom$InputMappings
  # OK for MappedDomain to be missing or blank, we don't use it

  # Keys can be empty, missing, or blank, unless there's a pre-merge domain (check later)
  if(!is.null(inp$Keys)){
    # check keys are parseable
    keys = unlist(strsplit(inp$Keys, "\\s*,\\s*"))
    if(!all(is_valid_variable_name(keys))){
      append.err(sprintf("Error D5: Domain '%s' Keys should be a comma separated list of valid column names.\n\tBad Keys: %s.",
                         dom$name, paste(keys[!is_valid_variable_name(keys)], collapse= ", ")))
    }
  }

  has.supp.file=F
  # check validity of premerge file if it exists and set a flag for ensuing premerge checks
  if(!is.empty.yaml(inp$PreMergeFile))
  {
    #check file path exists
    inp$PreMergeFile = printPath(inp$PreMergeFile)
    if(!file.exists(inp$PreMergeFile)){
      append.err(sprintf("Error D11: Domain '%s' PreMergeFile does not exist: %s",
                         dom$name, inp$PreMergeFile))
    } else{
      has.supp.file=T
    }
  }
  # Check formatting of pre-merge keys
  if(!is.empty.yaml(inp$PreMergeKeys) & has.supp.file){
    # check keys are parseable
    pmkeys = unlist(strsplit(inp$PreMergeKeys, "\\s*,\\s*"))
    if(!all(is_valid_variable_name(pmkeys))){
      append.err(sprintf("Error D6: Domain %s PreMergeKeys should be a comma separated list of valid column names.\n\tBad Keys: %s.",
                         dom$name, pmkeys[!is_valid_variable_name(pmkeys)]))
    }
  }

  # check formatting of premerge columns
  if(!is.empty.yaml(inp$PreMergeCols) & has.supp.file){
    # check premerge cols are parseable
    pmcols=unlist(strsplit(inp$PreMergeCols, "\\s*,\\s*"))
    for(col in pmcols){
      # verify format
      if(!grepl("\\w\\s*=\\s*\\w", col)){
        append.err(sprintf("Error D7: Domain '%s' PreMergeCols should be a comma separated list of column names and mappings (e.g. 'ID=USUBJID, TIME=EXSTDTC').\n\t%s.",
                           dom$name, paste0("Bad Format: ", col)))
      }

      tryCatch(parse(text=col), error=function(e) {
        append.err(sprintf("Error D10: Domain '%s' PreMergeCols should be a comma separated list of column names and mappings (e.g. 'ID=USUBJID, TIME=EXSTDTC').\n\t%s.",
                           dom$name, e$message))})
    }
  }

  # check formatting of premerge filter
  if(!is.empty.yaml(inp$PreMergeFilter) & has.supp.file){
    # check premerge cols are parseable
    tryCatch(parse(text=inp$PreMergeFilter), error=function(e){append.err(sprintf("Error D14: Domain %s PreMergeFilter is not a valid R expression.\n\t%s.",
                                                                                  dom$name, e$message))})
  }

  # check premerge supp
  if(!is.null(inp$PreMergeSupp) & has.supp.file){
    if(!is.logical(inp$PreMergeSupp)) {
      append.err(sprintf("Error D8: Domain '%s' PreMergeSupp (SUPPQUAL) flag must be TRUE or FALSE.  Current value: '%s' is not valid.",
                         dom$name, inp$PreMergeSupp))
    }
    #check that SUPPQUAL required columns are mapped
    if(inp$PreMergeSupp==T){
      must_map = c("QNAM","QVAL")
      if(inp$MappedDomain!="DM") must_map=c("IDVARVAL",must_map)
      if(!all(sapply(must_map, grepl, inp$PreMergeCols))){
        append.err(sprintf("Error D9: Domain '%s' has SUPPQUAL premerge and must map %s in the PreMergeCols", dom$name, must_map))
      }
    }
  }

  #LPK-363 add PreprocessHook
  # process preprocessing hook
  if(!is.empty.yaml(inp$PreprocessHook)){
    pre_hook = inp$PreprocessHook
    # Premerge hook is function body.  it must all parse as valid R expression
    tryCatch(parse(text=pre_hook), error=function(e){append.err(sprintf("Error D17: 'PreprocessHook' is not a valid R expression", e$message))})
  }

  ## check additional mapping columns
  for(aM in inp$Columns){
    #check format
    if(any(is.null(aM$Name), is.null(aM$Mapping))){
      append.err(sprintf("Error D12: Column mapping is missing Name or Mapping element in Domain '%s'.", dom$name))
    }
    else{
      ## is column name valid
      if(!is_valid_variable_name(aM$Name)){
        append.err(sprintf("Error D13: Column mapping name '%s' is not a valid R variable name.", aM$Name))
      }
      else{
        ## verify parsing of Mapping, blanks are not allowed
        if(is.blank(aM$Mapping)){append.err(sprintf("Error D14: Column mapping '%s' in Domain '%s' cannot be empty", aM$Mapping, dom$name))}

        # check the mapping
        checks = check_mapping(aM$Mapping, validnames)
        if(checks$parse_err) {append.err(sprintf("Error D15: Column mapping '%s=%s' in Domain '%s' is not a valid R expression: \n %s",
                                                 aM$Name, aM$Mapping, dom$name, checks$expr$message))}
        if(checks$name_err) {
          if(!is.empty.yaml(aM$Required)) {
            if(aM$Required){
            append.err(sprintf("Error D16: Column mapping '%s=%s' : \n names [%s] are not found in Domain '%s'",
                               aM$Name, checks$expr, paste(checks$bad_names,collapse=", "), dom$name))}
            else {
              warning(sprintf("Warning D16: Optional column mapping '%s=%s' : \n names [%s] are not found in Domain '%s'",
                              aM$Name, checks$expr, paste(checks$bad_names,collapse=", "), dom$name))
            }
          }

        }
        ## after checking, add the aM column name to valid names,
        # to allow subsequent columns that reference it to pass checks.
        # but only if validnames wasn't null to begin with, so keep in the name_err check
        if(!is.null(validnames)) validnames=c(validnames,aM$Name)
      }
    }
  }
  errs
}

# @rdname domain
#' @export
#'

is.domain = function(obj){
  inherits(obj, "domain")
}

#' Load a domain from a domain object or from settings
#'
#' @param x A domain object with a valid name and filename
#' @param settings Additional settings to pass to the loading function.
#' @param .fun An optional mapping function to process the domain after loading
#' for formatted text files.  If missing the function will be constructed
#' from the domain settings.
#' @param .hook An optional function to process the loaded source after
#' the pre-merge process (if applicable) and before calling .fun.  If missing
#' the function will be constructed from the domain settings.
#'
#' @details The domain source files are loaded and additional processing
#' is run.  First the main source is loaded.  Then, if a pre-merge file
#' is indicated, the pre-merge source is loaded and the pre-merge proceeds.
#' After the premerge the hook function is run, which allows for arbitrary
#' processing of the loaded data.  Metadata (inMeta) is captured at this point.
#' Then the mapping function (.fun) is run and final metadata are captured.
#'
#' @return An S3 object of class domain
#' @importFrom tools file_ext md5sum
#' @export
#'

load.domain = function(x, settings, .fun, .hook)
{
  #load the domain file specified by the domain object x
  #settings allows for overriding the current domain settings
  #.fun is a pre-processing function for the domain.
  x$Loaded=F
  error_context=""
  if(!x$valid){
    #don't bother loading if settings are invalid
    warning("Domain was not loaded due to errors in settings.")
    return(x)
  }
  x=tryCatch(
    {
      error_context="loading file"
      #check domain filename extension.  Should be one of: sas7bdat, xpt, xls, or {csv, txt, dat, ...}
      ftype = tools::file_ext(x$filepath)
      x$Data = switch(ftype,
                        sas7bdat = load_sas7bdat(x$filepath),
                        dat = ,
                        csv = ,
                        txt = load_table(x),
                        xls = ,
                        xlsx = load_excel(x),
                        xpt = load_xpt(x$filepath),
                        {stop(paste0("Domain: ", x$name, " - Cannot load files of type ", ftype))}
                        )
      error_context="running premerge"
      x=premerge.domain(x)

      error_context="running preprocessing hook"
      ## call the hook function
      if(!missing(.hook)){
        if(!is.null(.hook)){x$Data = .hook(x$Data)}

      } else {
        # not provided so get from fnPreProc
        eval(parse(text=x$fnHook)) # created locally here
        x$Data = eval(call(sprintf("preprocessHook_%s", x$name), x$Data))
      }
      error_context="getting source metadata"

      in.info=c(ncol=ncol(x$Data), nrow=nrow(x$Data), md5 = tools::md5sum(x$filepath), file.info(x$filepath))
      x$in.info=in.info
      x$inMeta = get_metadata(x$Data)
      x$Loaded=T
      x
    },
    error=function(cond){
      errmsg = sprintf("Error loading domain [%s] while %s:\n%s ",
                       x$name, error_context, cond)
      message(errmsg)
      x$errors = c(x$errors, errmsg)
      x$Loaded=F
      return(x)
    }
  )

  if(!x$Loaded){
    # get out if we failed to load the data
    message("\t Failed to load domain.  See <domain>$errors for details")
    x$outMeta=list()
    return(x)
  }

  # print file info
  cat(sprintf("\nDomain loaded: %s\n", x$name))
  cat(sprintf("\tSource File: %s\n\tDimensions: %s rows x %s columns\n\tMD5 sum: %s\n\tLast Modified: %s\n",
              x$filepath, in.info$nrow, in.info$ncol, in.info$md5, in.info$mtime))

  ## do the symantic check again here, if we fail, at least we have metadata
  x$SourceMetaData$Columns = x$inMeta
  x$SourceMetaData$Source = "manual"
  x$errors = c(x$errors, validate.domain(x))
  x$SourceMetaData=NULL
  if(length(x$errors)>0){
    message("\t Semantic errors detected. See <domain>$errors for details")
    x$valid=F
    x$outMeta=list() # to ensure no NULL passed back to GUI
    return(x)} #just get out here

  ## call the preprocessing function
  x=tryCatch({
    if(!missing(.fun)){
      if(!is.null(.fun)){
        x = .fun(x)
      }
    } else {
      # not provided so get from fnPreProc
      eval(parse(text=x$fnPreProc)) # created locally here
      x = eval(call(sprintf("preprocess_%s", x$name), x))
    }
    x},
    error=function(cond){
      message(paste("Error in mapping function for domain:", x$name))
      message(cond)
      x$errors = c(x$errors, paste("Error in mapping function:", cond))
      x$Loaded=F
      x$outMeta=list() # pass back empty list to trigger error
      return(x)
    }
  )
  if(!x$Loaded){return(x)} #get out due to failure in process

  # assign column properties
  for(col in x$InputMappings$Columns){
    cname=col$Name
    if(!is.null(x$Data[[cname]])){
      # if the column input mappings were specified, set attributes
      attributes(x$Data[[cname]])[c("type","units")] = list(type=col$Type, units=col$Units)
      # not if we have an unspecified column here get_metadata will infer type.  This could happen
      # if .fun is missing.
    }

  }

  # set the processed metadata
  x$outMeta = get_metadata(x$Data)
  out.info=list(ncol=ncol(x$Data), nrow=nrow(x$Data), timestamp=Sys.time())
  # print file info
  cat(sprintf("\tPre-processed Data [%s x %s] at %s\n", out.info$nrow, out.info$ncol, out.info$timestamp))
  x$in.info=in.info
  x$out.info=out.info
  x
}

#' Save meta data for a list of domains
#'
#' @param dom.l A list of domains for which to save metadata
#' @param filename A file into which to write the YAML metadata
#'
#' @export
#'

domains.save.metadata = function(dom.l, filename)
{
  #dom.l must be a list of domains

  #filename must be a writeable directory, check on write

  meta.l = list()
  for(dom in names(dom.l)){
    x=dom.l[[dom]]
    meta.l = c(meta.l, list(list(Domain=x$name, Source=x$filepath, Type="Raw", Columns=x$inMeta),
                            list(Domain=x$name, Source=x$filepath, Type="Processed", Columns=x$outMeta) ))
    }
  cat(yaml::as.yaml(meta.l), file=filename)
}

write_hook.Domain = function(node){
  # write the preprocessing function for a domain
  # returns stub if hook is NULL or empty

  inp = node$InputMappings
  paste0("preprocessHook_",node$name, " <- function(data){\n",
         inp$PreprocessHook,
         "\ndata\n}")

}

write_function.Domain = function(node){
  # write the mapping function for a domain
  txt = ""

  # write declaration "preprocess_XX <- function(dom){
  paste0(txt, "preprocess_",node$name, " <- function(dom){") -> txt

  inp = node$InputMappings
  if(!is.null(inp)){

    # write a filter statement, if there is a filter
    .filter=NULL
    if(!is.empty.yaml(inp$Filter)){
      .filter = sprintf(".filter=%s", inp$Filter)
    }

    # write pre-processing of columns
    if(!is.null(inp$Columns)){
      .cols = sapply(inp$Columns, function(x) paste0(x$Name,"=",x$Mapping))
      .cols = paste(c(.filter, .cols), collapse=", ")
      if(.cols!="") paste0(txt, "\ndom$Data = getDomain(dom$Data, ", .cols, ")") -> txt
    }

  }
  #close function
  paste0(txt,"\ndom\n}") -> txt
  txt

}

premerge.domain = function(node){
  # internal function that takes a domain node and does a pre-merge on it, if the settings exist
  inp = node$InputMappings
  if(!is.null(inp)){
    # check for pre-merge and write statement
    if(!is.empty.yaml(inp$PreMergeFile)){
      #paste0(txt,"\n #pre-merge \n") ->txt
      if(file.exists(inp$PreMergeFile)){
        #build settings for pre-merge domain
        #suppdom = domain(list(name=paste0("SUPP",node$name), filepath=printPath(inp$PreMergeFile)))
        suppdom = domain(list(name=paste0("SUPP",node$name), filepath=inp$PreMergeFile))
#         txt = sprintf("%sdomSupp = domain(list(name='SUPP%s', filepath='%s'))\n", txt, node$name,
#                       #normalizePath(inp$PreMergeFile, winslash = "\\", mustWork = F))
#                       printPath(inp$PreMergeFile))
        #tweak keys
        #.keys=inp$PreMergeKeys
        .keys=unlist(strsplit(inp$PreMergeKeys, "\\s*,\\s*"))
        #txt = sprintf("%skeys=c(%s)\n", txt, paste0(sprintf("'%s'", .keys), collapse=", "))
        # check if suppqual
        .supp=F
        if(!is.null(inp$PreMergeSupp)) .supp=inp$PreMergeSupp

        # check for filter
        .pmfilter=NULL
        if(!is.empty.yaml(inp$PreMergeFilter)) .pmfilter = inp$PreMergeFilter

        #check for column transformations
        .cols=NULL
        if(!is.null(inp$PreMergeCols)) .cols=eval(parse(text=sprintf("alist(%s)", inp$PreMergeCols)))

        #append the optional arguments (filter and cols)
#         .args = paste0(c(.pmfilter, .cols), collapse=", ")
#         if(.args!="") .args=paste0(", ", .args)

        #txt = sprintf("%sdom$Data = pre.merge(dom, domSupp, keys, supp=%s%s)\n", txt, .supp, .args)
        # call pre.merge directly
        #node$Data = pre.merge(inp, suppdom, keys, .filter=inp$PreMergeFilter, supp=.supp, lazy_dots(.cols))
        arglist = c(list(dom1=node, dom2=suppdom, .filter=.pmfilter, keys=.keys, supp=.supp), .cols)
        lapply(suppdom,print)
        # remove any NULL elements in list
        arglist=arglist[!sapply(arglist,is.null)]
        node$Data = do.call(pre.merge,arglist)
      }
    }
  }
  node
}

#' Create a domain object from file and mappings information
#'
#' @param name The name of the domain.
#' @param path The path to the domain data.
#' @param filetype The type of file, one of: sas7bdat (default), xpt, csv, txt,
#' dat, xls, or xlsx.
#' @param filename The name (without directory or extension) of the main domain file.
#' This is defaulted to the name argument.
#' @param premerge.filename The name of the premerge file.  If the premerge file
#' is a CDISC SDTM supplementary domain (defaulted with has.supp=T) then this filename
#' defaults to 'SUPP'+name (with the appropriate path and extension).
#' @param mappings A domain InputMappings object, perhaps loaded from a yaml file, with
#' which to override default settings.
#' @param has.supp Indicates if a CDISC SDTM supplementary domain should be configured
#' and searched for.
#' @param ... Additional overrides for file settings, which are passed to various
#' load routines.  For text files these include: sep (comma, semicolon, tab, whitespace);
#' header (T/F); fill (T/F); quote (none, single_quote, double_quote); comment.char;
#' skip.  For Excel files: sheet (name or number of the sheet to read); range
#' (the cell range to read).  To reshape a text or Excel file, provide the id_cols as a
#' comma separated single string (it will be parsed later) of column names to keep as
#' the unique identifiers for the rows.  Columns not in the list will be converted
#' to name/value pairs in new columns.
#'
#' @return A doman object.
#' @export
#'
#' @usage create.domain("EX")
#'

create.domain = function(name=NULL,
                         path=".",
                         filetype=c("sas7bdat","xpt","csv","txt", "dat","xls","xlsx"),
                         filename=NULL,
                         premerge.filename=NULL,
                         mappings=NULL,
                         has.supp=T,
                         ...){

  # Need to have a name, so check and error if it's blank
  if(is.null(name)){
    stop("domain name must be provided")
  }

  # check the path
  if(is.null(path)){
    # current working directory has the files
    path = getwd()
  }
  if(!dir.exists(path)) {
    stop(sprintf("directory [%s] does not exist", path))
  }

  # get filetype, set filename if not given
  filetype = match.arg(filetype)
  if(is.null(filename)){
    # assume default filename
    filename = file.path(path, paste0(name,".", filetype))
  }

  # check for existence of files
  if(!file.exists(filename)){
    stop(sprintf("domain source file [%s] does not exist",filename))
  }

  # default the pre-merge name if not given
  if(is.null(premerge.filename)){
    # assume default premerge file name
    premerge.filename=file.path(dirname(filename),
                                paste0("supp",basename(filename)))
    # if our assumed file doesn't exist just ignore
    if(!file.exists(premerge.filename)){
      premerge.filename=""
      has.supp=F
    }
  } else {
    # a name was given, so just check if no path and attach to path
    pmpath=dirname(premerge.filename)
    if(pmpath=="."){pmpath = path}
    # check for extension
    pmext = tools::file_ext(premerge.filename)
    if(pmext==""){pmext=filetype}
    fname = tools::file_path_sans_ext(basename(premerge.filename))
    # reconstruct the premerge.filename
    premerge.filename = file.path(pmpath,
                                  paste0(fname,".",pmext))
    # hard stop if provided file doesn't exist
    if(!file.exists(premerge.filename)){
      stop(sprintf("premerge source file [%s] does not exist",premerge.filename))
    }
  }

  # setup premerge fields
  PreMergeFile = premerge.filename
  PreMergeFilter=NULL
  PreMergeSupp=F
  PreMergeKeys=NULL
  PreMergeCols=NULL
  UniqueRecordKeys=""

  if(has.supp){
    PreMergeFilter=sprintf("RDOMAIN=='%s'", name)
    PreMergeSupp=T
    # set keys for STDM
    if(name=="DM"){
      # DM doesn't use SEQ
      PreMergeKeys = "USUBJID"
      PreMergeCols="QNAM=QNAM, QVAL=QVAL"

    } else {
      # DM doesn't use SEQ
      PreMergeKeys = sprintf("USUBJID, %sSEQ",name)
      PreMergeCols= sprintf("%sSEQ=as.numeric(IDVARVAL), QNAM=QNAM, QVAL=QVAL", name)
    }
  }

  # set up default file settings
  FileSettings = list(sep="comma", header=T, fill=T, quote="none", comment.char='',
                      skip='', sheet='', range='', id_cols='')
  #override FileSettings from ... args
  FileSettings = modifyList(FileSettings, list(...)[names(FileSettings)])

  #set InputMappings
  InputMappings = list(MappedDomain=name,
                       Filter=NULL,
                       Keys="ID",
                       Columns=NULL,
                       PreMergeFile=PreMergeFile,
                       PreMergeKeys=PreMergeKeys,
                       PreMergeFilter= PreMergeFilter,
                       PreMergeCols=PreMergeCols,
                       PreMergeSupp=PreMergeSupp)
  # get mappings passed in and grab Columns, DefaultQuerySettings
  if(!is.null(mappings)){
    InputMappings=modifyList(InputMappings, mappings, keep.null = T)
  }
  # create
  domain(list(name=name,
              filepath=filename,
              FileSettings = FileSettings,
              InputMappings=InputMappings))
}

#' Load mappings list from mappings file
#'
#' @param filename
#'
#' @return A named list of domain InputMappings
#' @importFrom purrr map_chr
#' @importFrom yaml yaml.load_file
#' @export
#'

load.domain.mappings = function(filename){
  maps = yaml::yaml.load_file(filename)[["DomainMappings"]]
  names(maps) = map_chr(maps, "MappedDomain")
  maps
}
