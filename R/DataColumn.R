

# DataColumn as numeric with attributes
DataColumn=function(obj,label,units=NULL){
  structure(obj,label=label,units=units,class=c("DataColumn",class(obj)))
}

#length.DataColumn = function(dc) NextMethod("length")
#"[.DataColumn" = function(dc,idx) NextMethod("[")
#"[<-.DataColumn" = function(dc,idx,val) NextMethod("[<-")
#"[[.DataColumn" = function(dc,idx) NextMethod("[[")
#"[[<-.DataColumn" = function(dc,idx,val) NextMethod("[[<-")


"*.DataColumn" = function(dc1,dc2) {
  a.1 = attributes(dc1)
  a.2 = attributes(dc2)
  units=paste(c(a.1$units,a.2$units),collapse="*")
  lab = paste(c(a.1$label,a.2$label),collapse="*")

  vals = as.numeric(dc1)*as.numeric(dc2)
  DataColumn(vals,label=lab,units=units)
}

"/.DataColumn" = function(dc1,dc2) {
  a.1 = attributes(dc1)
  a.2 = attributes(dc2)
  units=paste(c(a.1$units,a.2$units),collapse="/")
  lab = paste(c(a.1$label,a.2$label),collapse="/")

  vals = as.numeric(dc1)/as.numeric(dc2)
  DataColumn(vals,label=lab,units=units)
}

"+.DataColumn" = function(dc1,dc2) {
  #adds two numbers with consistent units and converts to units of second operand (dc2)
  a.1 = attributes(dc1)
  a.2 = attributes(dc2)
  if(!udunits2::ud.are.convertible(a.1$units,a.2$units)) stop(paste0("Cannot add ", a.1$units, " and ", a.2$units))
  vals = as.numeric(convert_units.DataColumn(dc1,a.2$units))+as.numeric(dc2)
  lab = paste(c(a.1$label,a.2$label),collapse="+")
  DataColumn(vals,label=lab,units=a.2$units)
}

"-.DataColumn" = function(dc1,dc2) {
  #subtracts two numbers with consistent units and converts to units of second operand (dc2)
  a.1 = attributes(dc1)
  a.2 = attributes(dc2)
  if(!udunits2::ud.are.convertible(a.1$units,a.2$units)) stop(paste0("Cannot subtract ", a.1$units, " and ", a.2$units))
  vals = as.numeric(convert_units.DataColumn(dc1,a.2$units))-as.numeric(dc2)
  lab = paste(c(a.1$label,a.2$label),collapse="-")
  DataColumn(vals,label=lab,units=a.2$units)
}

"^.DataColumn" = function(dc1,n) {
  #raises datacolumn to a power
  a.1 = attributes(dc1)
  if(!is.numeric(n)) stop("n must be numeric")
  un = a.1$units
  if(un!="") un = paste0(a.1$units,"^",n)

  vals = as.numeric(dc1)^n
  lab=""
  if(!is.null(a.1$label)) lab = paste(c("(",a.1$label,")^",n),collapse = "")
  DataColumn(vals,label=lab,units=un)
}

##Generics

print.DataColumn = function(dc1,...)  print(as.numeric(dc1),...)
format.DataColumn = function(dc1,...)  format(as.numeric(dc1),...)
is.DataColumn = function(x) inherits(x, "DataColumn")


##Non Generics
units.DataColumn = function(dc,...) attributes(dc)$units
label.DataColumn = function(dc,...) attributes(dc)$label

convert_units.DataColumn = function(dc,un) {
  cfactor =udunits2::ud.convert(1,units.DataColumn(dc),un)
  DataColumn(dc*cfactor,units=un,label=label.DataColumn(dc))
}

makeDataColumns = function(df,labels, units){
  dcs = lapply(seq_along(df),function(i) DataColumn(df[[i]],labels[i],units[i]))
  do.call(cbind,lapply(dcs,as.data.frame))
}

### test
# dc1 = DataColumn((1:10)/5,"DC1","apples")
# dc2 = DataColumn((1:10)*5,"DC2","oranges")
#
# dc1*dc2
#
# dc3 = DataColumn((1:10)*5,"DC3","ug")
# dc4 = DataColumn(rep(1,10),"DC4","L")
# dc5 = DataColumn(rep(1,10),"DC5","mg")
# dc6 = dc3/dc4/dc5*dc5
# units(dc6)
# dc6=convert_units.DataColumn(dc6,"mg/L")
# units(dc6)
#
# df = data.frame(A=1:10,B=2,C=3)
# makeDataColumns(df,labels=names(df),units=c("mg","g","L"))->df2
