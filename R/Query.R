# functions to check and make queries from yaml settings

is_valid_variable_name = function(x){
  ok = rep(T, length(x))
  ok[x == "..."] = F
  ok[grepl("^\\.{2}[[:digit:]]+$", x)] = F
  ok[x != make.names(x)] = F
  ok
}

write_function.Query = function(node, fn){
  # blank the output function text, and the collection of unit assignments
  txt = ""
  fn.sum=character()
  units=character()
  # insert the function name (passed from calling function)
  paste0(txt, fn, "(", node$Domain) -> txt
  # iterate over the nodes in the domain
  for(n in names(node)){
    # skip name and domain entries...
    if(n %in% c("Name", "Domain")) next
    # pass along top level settings for query to the get* function
    if(n!="Columns"){
      if(!is.empty.yaml(node[[n]])){
        paste0(txt, ", ",n,"=",node[[n]]) -> txt
        }
      }
    else{
      # if we have a mapped column, grab the mapping to pass along as part of ...
      # also get the units, if provided for subsequent setting of unit attributes
      for(i in node[[n]]){
        paste0(txt, ", ",i$Name,"=",i$Mapping) -> txt
        if(!is.empty.yaml(i$Units)){
          # there are units here, get them.
          if(grepl("MW",i$Units)){
            # If MW is provided in Units, be sure to set it up as a vector
            toks = tokenize(i$Units)
            units=c(units,sprintf("%s=c('%s', %s)",i$Name, toks[1], toks[2]))
          } else {
            units=c(units,sprintf("%s='%s'", i$Name, i$Units))
          }
        }
        # build up fun.summary for getCov and getCovT
        if(!is.empty.yaml(i$Summary) & fn %in% c("getCov","getCovT")){
          fn.sum =c(fn.sum,sprintf("%s='%s_'", i$Name, i$Summary))
        }
      }
    }
  }
  # bind the fn.sum values
  if(length(fn.sum)>0){
    fn.sum = sprintf("fun.summary = list(%s)", paste0(fn.sum, collapse=","))
    txt = paste0(txt, ", ", fn.sum)
  }
  # close out the function
  paste0(txt, ")") -> txt
  #if there are units in mapped columns, then chain to convert from list
  if(length(units)>0) sprintf("%s %%>%%\n#assign units\nconvert_units_from_list(.ul=list(%s))\n",
                              txt, paste0(units, collapse=",")) ->txt

  txt
}

#' Create a Query object
#'
#' @details The settings list in node is screened for syntactical errors such as missing mappings, incorrectly
#' formatted R names, and unparseable R expressions.  Errors are collected and returned in a list.
#' @param node A list with query settings
#' @param reqMaps Names of required mappings as character
#' @param optMaps Names of optional mappings as character
#' @param fn Name of the query function to create
#'
#' @return A Query object with 'mappings' member as the passed node.  Errors encountered during screening are
#' listed in the 'errors' member.
#' @importFrom udunits2 ud.is.parseable
#' @export
#'

Query = function(node, reqMaps=NULL, optMaps=NULL, fn=NULL, validnames=NULL){

  #helper function to determine if mapping is all whitespace
  is.blank = function(txt){grepl("^[[:space:]]*$", txt)}
  # create a Query object from a yaml node (as list)
  qry=node
  errs = list()
  append.err = function(err){
    errs<<-c(errs,err)
  }

  # add reqMaps and optMaps names to validnames
  validnames = c(validnames, reqMaps, optMaps)

  #check for required elements
  ## Name
  if(is.null(qry$Name)){
    append.err("Error Q1: Attempt to create Query with no name")
    qry$Name="unnamed"
  }
  else{
    # Name must be a legal R variable name
    if(!is_valid_variable_name(qry$Name)) append.err(sprintf("Error Q2: Query name '%s' is not a valid R variable name.", qry$Name))
  }
  ## Domain
  if(is.null(qry$Domain)){
    append.err(sprintf("Error Q3: 'domain' element of Query %s is missing.", qry$Name))
  }
  else{
    # Domain must be a legal R variable name
    if(!is_valid_variable_name(qry$Domain)) append.err(sprintf("Error Q4: in Query '%s'. '%s' is not a valid R domain name.", qry$Name, qry$Domain))
  }
  ## Filter
  if(!is.null(qry$Filter)){
    # check the mapping
    checks = check_mapping(qry$Filter, validnames)
    if(checks$parse_err) {append.err(sprintf("Error Q5: Filter mapping '%s' of Query '%s' is not a valid R expression: \n %s",
                                             qry$Filter, qry$Name, checks$expr$message))}
    if(checks$name_err) {append.err(sprintf("Error Q6: Filter mapping '%s' of Query '%s': \n names [%s] not found in Domain '%s'",
                                            qry$Filter, qry$Name,  paste(checks$bad_names,collapse=", "), qry$Domain))}
   # filter must parse without error.  Whitespace in filter will process as missing in get*

  }
  if(!is.empty.yaml(qry$Units)){
    # allow a column name or a parseable unit string
    if(!ud.is.parseable(qry$Units)){
      # not a unit, so is it a column?
      if(!qry$Units %in% validnames){
        append.err(sprintf("Error Q18: Units mapping '%s' of Query '%s': is neither a valid column name nor a parseable unit string.",
                           qry$Units, qry$Name))}
    }
  }

  ## check required mappings
  for(rM in reqMaps){
    ## check for required Mapping element
    if(is.null(qry[[rM]])){
      append.err(sprintf("Error Q7: Required mapping '%s' of Query '%s' is missing", rM, qry$Name))
    }
    else{

      ## verify parsing, fail on whitespace
      if(is.blank(qry[[rM]])){
        append.err(sprintf("Error Q8: Required mapping '%s' of Query '%s' cannot be empty", rM, qry$Name))
      }
      else {
        # check the mapping
        checks = check_mapping(qry[[rM]], validnames)

        if(checks$parse_err) {append.err(sprintf("Error Q9: Required mapping '%s=%s' of Query '%s' is not a valid R expression: \n %s",
                                                 rM, qry[[rM]], qry$Name, checks$expr$message))}
        if(checks$name_err) {append.err(sprintf("Error Q10: Required mapping '%s=%s' of Query '%s' expression: \n %s \n names [%s] not found in Domain '%s'",
                                                rM, qry[[rM]], qry$Name, checks$expr, paste(checks$bad_names,collapse=", "), qry$Domain))}

        }

      }
  }
  ## check optional mappings
  for(oM in optMaps){
    ## check for optional Mapping element
    if(!is.null(qry[[oM]])){
      # check the mapping
      checks = check_mapping(qry[[oM]], validnames)
      if(checks$parse_err) {append.err(sprintf("Error Q11: Optional mapping '%s=%s' of Query '%s' is not a valid R expression: \n %s",
                                               oM, qry[[oM]], qry$Name, checks$expr$message))}
      if(checks$name_err) {append.err(sprintf("Error Q12: Optional mapping '%s=%s' of Query '%s' expression: \n %s \n names [%s] not found in Domain '%s'",
                                              oM, qry[[oM]], qry$Name, checks$expr, paste(checks$bad_names,collapse=", "), qry$Domain))}
    }

  }

  ## special check for CMT in dosing
  if(!is.empty.yaml(qry$CMT) & fn=="getIndividualDoses"){
    # check the mapping
    oM = "CMT"
    checks = check_mapping(qry[[oM]], validnames)
    commalist = all(grepl("^((\\s*\\w+\\s*),)*(\\s*\\w+\\s*)+$",qry[[oM]]))
    # it doesn't parse err, but is a comma separated list it is OK

    if(checks$parse_err & !commalist) {append.err(sprintf("Error Q11: Optional mapping '%s=%s' of Query '%s' is not a valid R expression or comma separated list: \n %s",
                                             oM, qry[[oM]], qry$Name, checks$expr$message))}
    else if(checks$parse_err & commalist) { qry$CMT=sprintf("'%s'",qry$CMT) }#have to put quotes around it here
    if(checks$name_err) {append.err(sprintf("Error Q12: Optional mapping '%s=%s' of Query '%s' expression: \n %s \n names [%s] not found in Domain '%s'",
                                            oM, qry[[oM]], qry$Name, checks$expr, paste(checks$bad_names,collapse=", "), qry$Domain))}
  }

  ## check additional mapping columns
  for(aM in qry$Columns){
    #check format
    if(any(is.null(aM$Name), is.null(aM$Mapping))){
      append.err(sprintf("Error Q13: Additional mapping is missing Name or Mapping element in Query %s.", qry$Name))
    }
    else{
      ## is column name valid
      if(!is_valid_variable_name(aM$Name)){
        append.err(sprintf("Error Q14: Additional mapping '%s' in Query '%s': column name is not a valid R variable name.", aM$Name, qry$Name))
      }
      else{
        ## verify parsing of Mapping, blanks are not allowed
        if(is.blank(aM$Mapping)){append.err(sprintf("Error Q15: Additional mapping '%s' in Query '%s' cannot be empty.", aM$Name, qry$Name))}
        # check the mapping
        checks = check_mapping(aM$Mapping, validnames)
        if(checks$parse_err) {append.err(sprintf("Error Q16: Additional mapping '%s=%s' of Query '%s' is not a valid R expression: \n %s",
                                                 aM$Name, aM$Mapping, qry$Name, checks$expr$message))}
        if(checks$name_err) {append.err(sprintf("Error Q17: Additional mapping '%s=%s' of Query '%s' expression: \n %s \n names [%s] not found in Domain '%s'",
                                                aM$Name, aM$Mapping, qry$Name, checks$expr, paste(checks$bad_names,collapse=", "), qry$Domain))}
        ## after checking, add the aM column name to valid names,
        #  to allow subsequent columns that reference it to pass checks
        # but not if validnames is NULL
        if(!is.null(validnames)) validnames=c(validnames,aM$Name)

        }

      }
    }

  valid=F
  if(length(errs)==0) valid=T

  fnCall=NULL
  if(valid & !is.null(fn)) fnCall = write_function.Query(qry,fn)

  structure(list(mappings=qry, valid=valid, errors=errs, fnCall = fnCall) , class="Query")
}

#' @describeIn Query Create Dependent Variable Query object
#' @export
#'
Query_DV = function(node, meta_data=NULL){
  ## create Query object
  reqMaps = c("ID", "TIME", "DV")
  optMaps = c("EVID", "BQL", "LLOQ", "MDV", "CMT", "dv.filter")

  # get the metadata
  vnames = NULL
  if(!is.null(meta_data)){
    vnames = c(get_columns_metadata(meta_data),".") #add . for some chaining functions to use
  }
  Query(node, reqMaps=reqMaps, optMaps = optMaps, fn="getDV", validnames = vnames)
}

#' @describeIn Query Create Static Covariate Query object
#' @export

Query_Cov = function(node, meta_data=NULL){
  ## create Query object
  reqMaps = c("ID", "cov.keys")
  optMaps = c("cov.val", "cov.col", "cov.filter")

  # get the metadata
  vnames = NULL
  if(!is.null(meta_data)){
    vnames = c(get_columns_metadata(meta_data),".") #add . for some chaining functions to use
    if(!is.empty.yaml(node$cov.col)) vnames = c(vnames, get_column_values_metadata(meta_data$Columns[[node$cov.col]]))
  }

  if(!is.null(node$cov.keys)){
    #tweak keys
    keys=unlist(strsplit(node$cov.keys, "\\s*,\\s*"))
    node$cov.keys = sprintf("c(%s)", paste0(sprintf("'%s'",keys), collapse=", "))
    }
  Query(node, reqMaps=reqMaps, optMaps = optMaps, fn="getCov", validnames = vnames)
}

#' @describeIn Query Create Time-varying Covariate Query object
#' @export

Query_CovT = function(node, meta_data=NULL){
  ## create Query object
  reqMaps = c("ID", "TIME")
  optMaps = c("EVID", "covT.filter", "covT.val", "covT.col")
  # get the metadata
  vnames = NULL
  if(!is.null(meta_data)){
    vnames = c(get_columns_metadata(meta_data),".") #add . for some chaining functions to use
    if(!is.empty.yaml(node$covT.col)) vnames = c(vnames, get_column_values_metadata(meta_data$Columns[[node$covT.col]]))
  }
  Query(node, reqMaps=reqMaps, optMaps = optMaps, fn="getCovT", validnames = vnames)
}

#' @describeIn Query Create Individual Dosing Query object
#' @export
#'
Query_IndDose = function(node, meta_data=NULL){
  ## create Query object
  reqMaps = c("ID", "TIME", "AMT")
  optMaps = c("EVID", "II", "MEX", "SPLIT", "ex.filter")
  # get the metadata
  vnames = NULL
  if(!is.null(meta_data)){
    vnames = c(get_columns_metadata(meta_data),".") #add . for some chaining functions to use
  }
  Query(node, reqMaps=reqMaps, optMaps = optMaps, fn="getIndividualDoses", validnames=vnames)
}

get_mappings.Query = function(qry, meta_data, source_maps){
  #qry is a query object and dom is its referenced domain
  # get domain column names, parameter names available in domain
  dom_names = get_columns_metadata(meta_data)
  parameter = coalesce(qry$cov.col,qry$covT.col) #only one is not null
  values = coalesce(qry$cov.val,qry$covT.val) #only one is not null
  param_names = if(!is.empty.yaml(parameter)){
    get_column_values_metadata(meta_data$Columns[[parameter]])
  } else NULL

  names(source_maps) = purrr::map(source_maps,"Name") #put the names in for easier access


  maps = list()

  # get required and optional mappings from list here, but only those present in qry
  fixmaps = names(qry)[names(qry) %in% c("ID","TIME","EVID","AMT","DV","CMT","BQL","LLOQ","MDV","II","MEX")]
  keys=NULL
  if(!is.empty.yaml(qry[["cov.keys"]])) keys=paste(eval(parse(text=qry[["cov.keys"]])),collapse=" + ")
  # record the conditions of the qry.  made up of filter and if present EVID, CMT, and keys
  cond = paste(c(qry$Filter, sprintf("EVID==%s", qry[["EVID"]]), sprintf("CMT==%s", qry[["CMT"]]),
                 sprintf("Join on: %s", keys)), collapse=" & ")

  for(col in fixmaps){
    if(!is.empty.yaml(qry[[col]])){
      # for each of the fixed mappings get the variables and condition (e.g. filter)
      vars = all.vars(parse(text=qry[[col]]))
      # vars that are columns in domain data (only option for fixed mappings)
      varsx = purrr::map_chr(source_maps[intersect(vars,dom_names)],
                            function(x)sprintf("%s=%s",x$Name, x$Mapping )) # grab the source mappings

      maps[[col]] = list(Column=col,
                         Domain=qry$Domain,
                         Mapping=qry[[col]],
                         Sources=paste(varsx, collapse=", "),
                         Source_Columns = paste(vars, collapse=", "),
                         Condition=cond)
    }
  }
  # do same for mapped columns
  for(col in qry$Columns){
    # for each of the column mappings get the variables
    vars = all.vars(parse(text=col$Mapping))
    # vars that are columns
    varsx = purrr::map_chr(source_maps[intersect(vars,dom_names)],
                           function(x)sprintf("%s=%s",x$Name, x$Mapping )) # grab the source mappings
    # vars that are in the param_names, show mapped name with
    if(!is.empty.yaml(parameter))
      varsx = c(varsx, purrr::map_chr(intersect(vars,param_names),
                                      function(x)sprintf("%s=%s[%s==%s]",x,values,parameter,x)))

    maps[[col$Name]] = list(Column=col$Name,
                            Domain=qry$Domain,
                            Mapping=col$Mapping,
                            Sources=paste(varsx, collapse=", "),
                            Source_Columns = paste(vars, collapse=", "),
                            Condition=cond)
  }
  maps
}

check_mapping = function(mapping, validnames){
  parse_err=T
  name_err=F
  bad_names = character()
  r_expr=tryCatch(parse(text=mapping), error=function(e) e)
  if(!inherits(r_expr, "error")) parse_err=F
  # if parses successfull, and given a list of names to check against
  if(!inherits(r_expr, "error") & !is.null(validnames)){
    # mapping gets parsed and the names checked against validnames
    m_vars = all.vars(r_expr) # get the variable names
    bad_names = setdiff(m_vars,validnames) # check the names against the available names
    # remove bad_names that are found as function names
    if(length(bad_names)) bad_names = bad_names[!unlist(lapply(bad_names, function(x)as.logical(length(find(x)))))]
    name_err = length(bad_names)>0
    # can add a check for functions, but need to look in all installed packages
  }
  list(parse_err=parse_err, expr=r_expr, name_err=name_err, bad_names=bad_names)
}
