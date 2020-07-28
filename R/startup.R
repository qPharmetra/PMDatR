# Startup code

.onLoad <- function(libname, pkgname){
  #check and set default options

  if(getOption("stringsAsFactors")) {
    options(stringsAsFactors = F)
  }

}

.onAttach <- function(libname, pkgname){
  packageStartupMessage(paste0("Welcome to PMDatR, ",Sys.getenv("USERNAME")))
  packageStartupMessage(paste("package path =", path.package("PMDatR")))
  packageStartupMessage("Installation Qualification disabled.")
  packageStartupMessage("setting stringsAsFactors to FALSE as PMDatR default")
  if(getOption("stringsAsFactors")) {
    options(stringsAsFactors = F)
  }
  if(is.null(getOption("qpExampleDir"))) options(qpExampleDir=file.path(path.package("PMDatR"),"NONMEM"))
}
