# Startup code

.onLoad <- function(libname, pkgname){
  #check and set default options

  if(getOption("stringsAsFactors", FALSE)) {
    options(stringsAsFactors = F)
  }

}

.onAttach <- function(libname, pkgname){
  packageStartupMessage(paste0("Welcome to PMDatR, ",Sys.getenv("USERNAME")))
  packageStartupMessage(paste("package path =", path.package("PMDatR")))
  packageStartupMessage(paste("library paths:\n", paste(.libPaths(), collapse="\n")))
  packageStartupMessage(paste(R.installation.qualification()$IQ.message))
  packageStartupMessage("setting stringsAsFactors to FALSE as PMDatR default")
  if(getOption("stringsAsFactors")) {
    options(stringsAsFactors = F)
  }
  if(is.null(getOption("qpExampleDir"))) options(qpExampleDir=file.path(path.package("PMDatR"),"NONMEM"))
}
