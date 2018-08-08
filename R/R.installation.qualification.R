

#' Title
#'
#' @return data.frame containing the dependencies for PMDatR
#'
#' @import utils
.getDependencyList = function()
{
  packs = tools::package_dependencies("PMDatR",installed.packages(),recursive=T)
  packs = unique(unlist(packs))
  packs = packs[order(packs)]
  deps.df = data.frame(installed.packages()[packs,])[,c("Package","Version","Priority")]
  #remove base and recommended
  deps.df = subset(deps.df,!Priority %in% c("base","recommended"),select=c("Package","Version"))
  ours.df = as.data.frame(as.list(installed.packages()["PMDatR",]))[,c("Package","Version")]
  rbind(deps.df,ours.df)
}


#' Run installation qualification for PMDatR in current R environment
#'
#' @return A list containing: IQ.message; packages.df - names of versions of packages that are out of compliance
#' @export
#'

R.installation.qualification = function()
{
#   find.in.path = function(path,fname){
#     parts = unlist(strsplit(path, .Platform$file.sep))
#     if(length(parts)==0) return("") #just return empty directory
#     if(!file.exists(file.path(path,fname)))
#       find.in.path(do.call(file.path, as.list(parts[1:length(parts) - 1])),fname)
#     else path
#   }
#

######
  #when updating req.packages run the following lines of code to get required packages and current versions
  # start, get package dependencies (note: only use this from build environment!)
  #write.csv(.getDependencyList(),"./inst/Qualification/package.versions.csv",row.names = F)
  # end, get package dependencies
######


  ## check R version, against project requirement, or against dependency requirement
  current.R = getRversion()
  required.R = "3.3.3"
  IQ.Rversion = (current.R != required.R)

  ## load R object with installed versions
  current.packs = installed.packages()
  rownames(current.packs)=NULL
  current.packs = data.frame(current.packs)
  current.packs = subset(current.packs, !Priority %in% c("base","recommended"),
                         select=c("Package","LibPath","Version"))

  allowed.packs = read.csv(system.file("Qualification/package.versions.csv", package="PMDatR"))
  allowed.packs = allowed.packs[,c("Package","Version")]


  #compare versions of packages installed, vs. what was in qpRepos during release of this PMDatR version
  compare.packs = merge(current.packs,allowed.packs,suffixes=c(".current",".allowed"),by="Package",all.x=T)


  compare.packs$extraneous = is.na(compare.packs$Version.allowed)
  compare.packs$bad.version = compare.packs$Version.current!=compare.packs$Version.allowed
  IQ.pack.versions = sum(compare.packs$bad.version,na.rm=T)>0
  IQ.pack.extras = sum(compare.packs$extraneous)>0
  ext.packs = compare.packs$Package[compare.packs$extraneous]
  ext.packs = paste(ext.packs, collapse="\n")
  bad.vers = compare.packs[compare.packs$bad.version==T & !is.na(compare.packs$bad.version),]
  bad.vers$message = sprintf("%s is %s expected %s",
                             bad.vers$Package,
                             bad.vers$Version.current,
                             bad.vers$Version.allowed)
  bad.vers = paste(bad.vers$message, collapse="\n")
  IQ.message = ifelse(IQ.Rversion | IQ.pack.versions,"IQ FAILED","IQ PASSED")
  if(IQ.Rversion) IQ.message = paste(IQ.message, "Incorrect R Version",sep="\n")
  if(IQ.pack.versions) IQ.message = paste(IQ.message, "Incorrect Package Versions:", bad.vers, sep="\n")
  if(IQ.pack.extras) IQ.message = paste(IQ.message, "Warning: extraneous packages detected", ext.packs, sep="\n")

  return(list(IQ.message = IQ.message, packages.df = compare.packs))

}
