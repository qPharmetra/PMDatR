#' safely coerce to numeric
as_numeric <- function(...) {
  as.numeric(as.character(...))
}

is.empty.yaml = function(node){
  # return T if node is null or empty string
  # node is a list object created from yaml (or really any list)
  if(is.null(node)) return(T)
  if(length(node)==0) return(T)
  if(all(node=="")) return(T)
  return(F)
}

load_yaml_file = function(settings_file){
  # yaml integer data handler
  # this fixes a situation where a numeric looking string is parsed from yaml but errors
  # due to being out of numeric range.  Brings it in as character instead.
  int_handler = function(x){
    if(is.na(suppressWarnings(as.integer(x)))) {
      as.character(x)
    } else {
      as.integer(x)}
  }

   yaml::yaml.load_file(settings_file, handlers=list("int"=int_handler))

}

is_whitespace = function(x) grepl("^\\s*$", x)

is_valid_format_spec = function(x) grepl("^%\\d*?(.\\d*)?[fg]$",x)
# is_valid_format_spec(c("%1.1g", "no", "%.4g", "%.3f","%1g", "%1.1f %s", "%s"))

coalesce = function(...){
  # return the first non-null argument, motivated by microbenchmark::coalesce
  # useful when we have default values and possible overrides
  Find(function(x){!is.null(x)}, list(...))
}

tokenize = function(x){
  # x is a comma delimited character list.  put quotes around the tokens [a, b] -> ["a", "b"]
  unlist(strsplit(x, "\\s*,\\s*"))
}
### utilities for diagnostics

lunique = function (x) length(unique(x))

sunique = function (x) unique(x)[order(unique(x))]

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

# given a column with potentially delimited values, duplicate rows for those values.
# separate_rows = function(df, col, sep="\\s*,\\s*"){
#   # NSE for column name
#   .col=deparse(substitute(col))
#   # sep can be a regex, so handle backslashes.  Split the column on the sep, creates a list column
#   expr = sprintf("strsplit(as.character(%s), '%s')", .col, gsub("\\\\","\\\\\\\\", sep))
#   #unnest the list column
#   df %>% mutate_(.dots=setNames(list(expr), .col)) %>% tidyr::unnest_(.col)
# }

special.range = function(x, unique.value, sep = " - ")
{
  ifelse(lunique(x)<=1
         , ifelse(!missing(unique.value), unique.value, paste(unique(x)))
         , paste(sprintf("%.3f",range(x, na.rm=T)), collapse = sep)
  )
}

print.report = function(x) print(x[[1]] %>% as.data.frame)

head.report = function(x) head(x[[1]] %>% as.data.frame)

str.report = function(x) str(x[[1]] %>% as.data.frame)

combine.dotvars = function(...)
{
  ## combine quote and unquoted to comma-delimited string
  ## for further evaluation
  unlist(
    lapply(
      lazyeval::lazy_dots(...)
      , function(x) paste(as.character(x$expr),collapse = "")
    )
  )
}

# internal function to convert path from backslashes to forward slashes
printPath = function(x) gsub("\\","/",x,fixed=T)


# internal function to reinterpret some common R errors

reinterpret_errors = function(x, header= 4){
  # first replace any leading hashes
  x=gsub("^#*\\s*","",x)

  # ignore any deprecation warnings
  if(grepl(".+deprecated.*", x)){ return() }

  # now look for common error patterns and replace them
  if(grepl("Error in .+: object '.*' not found",x)) x= gsub("Error in .+:","Error RT1:", x)
  if(grepl("Error in .+: could not find function",x)) x= gsub("Error in .+:","Error RT2:", x)
  if(grepl("Error in file(.+): cannot open the connection",x))
    x= gsub("Error in file(.+):","Error RT3: cannot open specified file", x)
  if(grepl("Error in .+: subscript out of bounds",x)){
    y=stringr::str_match(x,"Error in (.+): subscript out of bounds")
    x=sprintf("Error RT4: the index used in '%s' is outside the bounds of the variable", y[2])
  }
  if(grepl("Error in if.+: argument is not interpretable as logical",x)){
    y=stringr::str_match(x,"Error in if\\((.+)\\) : argument is not interpretable as logical")
    x=sprintf("Error RT5: in the if statement, '%s' must evaluate to TRUE or FALSE", y[2])
  }
  if(grepl("Error in .+: object .+ is not subsettable",x)){
    y=stringr::str_match(x,"Error in (.+)\\[")
    x=sprintf("Error RT6: '%s' is a function, but is being used like a vector (with square brackets)", y[2])
  }
  if(grepl("Error .+\\$ operator is invalid for atomic vectors",x)){
    y=stringr::str_match(x,"Error in (.+)\\$")
    x=sprintf("Error RT7: '%s' is not a list.  Try using square brackets instead of $ to access elements", y[2])
  }


  ## Warnings
  if(grepl("Warning in file\\((.+)\\): cannot open file .+: No such file or",x)){
    y=stringr::str_match(x,"cannot open file '(.+)'")
    x=sprintf("Warning RW1: The file or directory '%s' cannot be opened - it does not exist", y[2])
  }
  if(grepl("Warning in file\\((.+)\\): cannot open file .+: Permission denied",x)){
    y=stringr::str_match(x,"cannot open file '(.+)'")
    x=sprintf("Warning RW2: The file or directory '%s' cannot be opened - it is already open or is read only", y[2])
  }
  if(grepl("Warning .+: Post Merge: fun\\..+",x)){
    y=stringr::str_match(x,"Warning .+: (Post Merge[\\S\\s]+)") #regex matches . including  newlines
    x=sprintf("Warning RW3: %s", y[2])
  }

  #sprintf("\n%s %s\n", paste(rep("#",header),collapse=""), x)
  sprintf("<h%i>%s</h%i>", header,x, header)
}

