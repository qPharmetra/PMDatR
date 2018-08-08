# test the get* functions to retrieve data from domains

library(testthat)

library(parsedate)
library(dplyr)
library(tidyr)
library(PMDatR)

context("domain get* functions")

### TEST getting doses
# individual doses EX data set
dosedays = c("2011-03-27T08:30:00", "2011-04-01T9:27:00", "2011-04-12T7:49:00")
pos.dosedays=parsedate::parse_date(dosedays)
ch.dosedays = as.character(pos.dosedays) #"2011-03-27 08:30:00" "2011-04-01 09:27:00" "2011-04-12 07:49:00"
ex.df = data.frame(USUBJID=1:3, EXSTDTC=dosedays, EXENDTC=dosedays, EXDOSE=c(100,100,0), VISIT="TREATMENT DAY 1")


test_that("getIndividualDoses works with ID, TIME, AMT provided",{
  ex.get.df = getIndividualDoses(ex.df,ID=USUBJID,TIME=parsedate::parse_date(EXSTDTC),AMT=EXDOSE)
  #EVID gets defaulted to 1
  #MDV gets defaulted to 1
  expect_equal(names(ex.get.df),c("ID","TIME","AMT","EVID", "MDV")) #check column names
  expect_equal(ex.get.df$ID,1:3) #check ID
  expect_equal(as.character(ex.get.df$TIME), ch.dosedays) # check dates
  expect_equal(ex.get.df$EVID,rep(1,3)) #check EVID
  expect_equal(ex.get.df$MDV,rep(1,3)) #check MDV
})

test_that("getIndividualDoses works with extra column",{
  ex.get.df = getIndividualDoses(ex.df,ID=USUBJID,TIME=parsedate::parse_date(EXSTDTC),AMT=EXDOSE, VISIT=VISIT)
  expect_true("VISIT" %in% names(ex.get.df))
  expect_equal(ex.get.df$VISIT,rep("TREATMENT DAY 1",3)) #check VISIT
})

test_that("getIndividualDoses works with extra column as constant",{
  ex.get.df = getIndividualDoses(ex.df,ID=USUBJID,TIME=parsedate::parse_date(EXSTDTC),AMT=EXDOSE, DOMAIN="EX")
  expect_true("DOMAIN" %in% names(ex.get.df))
  expect_equal(ex.get.df$DOMAIN,rep("EX",3)) #check DOMAIN
})

test_that("getIndividualDoses works with filter",{
  ex.get.df = getIndividualDoses(ex.df,ID=USUBJID,TIME=parsedate::parse_date(EXSTDTC),
                                 AMT=EXDOSE, DOMAIN="EX",
                                 ex.filter=USUBJID==1)
  expect_equal(names(ex.get.df),c("ID","TIME","AMT","EVID", "MDV", "DOMAIN")) #check column names
  expect_equal(ex.get.df$ID,1) #check ID
  expect_equal(as.character(ex.get.df$TIME), ch.dosedays[1]) # check dates
  expect_equal(ex.get.df$EVID,rep(1,1)) #check EVID
  expect_equal(ex.get.df$MDV,rep(1,1)) #check MDV
})

test_that("getIndividualDoses warns with zero row filter",{
  expect_warning(ex.get.df <- getIndividualDoses(ex.df,ID=USUBJID,TIME=parsedate::parse_date(EXSTDTC),
                                 AMT=EXDOSE, DOMAIN="EX",
                                 ex.filter=USUBJID==0))
  expect_equal(ex.get.df,data_frame())
})

test_that("getIndividualDoses errors with bad row filter",{
  expect_error(ex.get.df <- getIndividualDoses(ex.df,ID=USUBJID,TIME=parsedate::parse_date(EXSTDTC),
                                                 AMT=EXDOSE, DOMAIN="EX",
                                                 ex.filter=arglebargle(USUBJID)), "*unable to process ex.filter*")
})

filtfun = function(){1}
test_that("getIndividualDoses works with global function in filter",{
  ex.get.df = getIndividualDoses(ex.df,ID=USUBJID,TIME=parsedate::parse_date(EXSTDTC),
                                 AMT=EXDOSE, DOMAIN="EX",
                                 ex.filter=USUBJID==filtfun())
  expect_equal(names(ex.get.df),c("ID","TIME","AMT","EVID", "MDV", "DOMAIN")) #check column names
  expect_equal(ex.get.df$ID,1) #check ID
  expect_equal(as.character(ex.get.df$TIME), ch.dosedays[1]) # check dates
  expect_equal(ex.get.df$EVID,rep(1,1)) #check EVID
  expect_equal(ex.get.df$MDV,rep(1,1)) #check MDV
})

funfun = function(){"EX"}
test_that("getIndividualDoses works with global function in ...",{
  ex.get.df = getIndividualDoses(ex.df,ID=USUBJID,TIME=parsedate::parse_date(EXSTDTC),
                                 AMT=EXDOSE, DOMAIN=funfun(),
                                 ex.filter=USUBJID==filtfun())
  expect_equal(names(ex.get.df),c("ID","TIME","AMT","EVID", "MDV", "DOMAIN")) #check column names
  expect_equal(ex.get.df$ID,1) #check ID
  expect_equal(as.character(ex.get.df$TIME), ch.dosedays[1]) # check dates
  expect_equal(ex.get.df$EVID,rep(1,1)) #check EVID
  expect_equal(ex.get.df$MDV,rep(1,1)) #check MDV
})

test_that("getIndividualDoses works without ID provided errors",{
  expect_error(getIndividualDoses(ex.df,TIME=parsedate::parse_date(EXSTDTC),AMT=EXDOSE))
})

test_that("getIndividualDoses works without TIME provided errors",{
  expect_error(getIndividualDoses(ex.df,ID=USUBJID,AMT=EXDOSE))
})

test_that("getIndividualDoses works without AMT provided errors",{
  expect_error(getIndividualDoses(ex.df,ID=USUBJID,TIME=parsedate::parse_date(EXSTDTC)))
})

test_that("getIndividualDoses errors with bad mapping",{
  expect_error(getIndividualDoses(ex.df,ID=USUBJID,AMT=100,TIME=arglebargle(EXSTDTC)),"*Error in column*")
})


### Test getting DVs
sample.times = c(-.5,.5,1,2)
dv = c(NA,.5,1,2)
dvc = c("BQL",.5,1,2)
dtc.df = expand.grid(ID=1:3, times=sample.times)
pc.df = merge(dtc.df,data.frame(ID=1:3,date=pos.dosedays))
pc.df = cbind(pc.df, PCSTRESN=dv, PCSTRESC=dvc)
pc.df = transmute(pc.df, USUBJID=ID, PCDTC=parsedate::format_iso_8601(as.character(date+times*3600)),
                  PCTEST="ANALYTE1", PCCAT="PHARMACOKINETIC CONCENTRATION",
                  PCTPTNUM=times, VISIT="TREATMENT DAY 1", PCSTRESN=PCSTRESN, PCSTRESC=PCSTRESC)
#knock the tz info off the date
pc.df$PCDTC = gsub("\\+00:00","",pc.df$PCDTC)
times = pc.df$PCDTC


test_that("getDV works with ID, TIME provided",{
  pc.get.df = getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC), DV=PCSTRESN)
  #EVID gets defaulted to 0
  #MDV gets defaulted to 0
  expect_equal(names(pc.get.df), c("ID","TIME","DV","EVID","MDV")) #check column names
  expect_equal(pc.get.df$ID, rep(1:3, each=4)) #check ID
  expect_equal(pc.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(pc.get.df$DV, rep(dv, 3)) #check DV
  expect_equal(pc.get.df$EVID, rep(0, 12)) #check EVID
  expect_equal(pc.get.df$MDV, rep(0, 12)) #check MDV
})

test_that("getDV works with filter",{
  pc.get.df = getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC), DV=PCSTRESN,
                    dv.filter=USUBJID==1)
  #EVID gets defaulted to 0
  #MDV gets defaulted to 0
  expect_equal(names(pc.get.df), c("ID","TIME","DV","EVID","MDV")) #check column names
  expect_equal(pc.get.df$ID, rep(1, each=4)) #check ID
  expect_equal(pc.get.df$TIME, parsedate::parse_date(times[1:4])) #check TIME
  expect_equal(pc.get.df$DV, rep(dv, 1)) #check DV
  expect_equal(pc.get.df$EVID, rep(0, 4)) #check EVID
  expect_equal(pc.get.df$MDV, rep(0, 4)) #check MDV
})

test_that("getDV warns with zero row filter",{
  expect_warning(pc.get.df <- getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC), DV=PCSTRESN,
                                     dv.filter=USUBJID==0))
  expect_equal(pc.get.df,data_frame())
})

test_that("getDV errors with bad row filter",{
  expect_error(pc.get.df <- getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC), DV=PCSTRESN,
                                 dv.filter=arglebargle(USUBJID)), "*unable to process dv.filter*")
})

test_that("getDV works with global function as filter",{
  pc.get.df = getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC), DV=PCSTRESN,
                    dv.filter=USUBJID==filtfun())
  #EVID gets defaulted to 0
  #MDV gets defaulted to 0
  expect_equal(names(pc.get.df), c("ID","TIME","DV","EVID","MDV")) #check column names
  expect_equal(pc.get.df$ID, rep(1, each=4)) #check ID
  expect_equal(pc.get.df$TIME, parsedate::parse_date(times[1:4])) #check TIME
  expect_equal(pc.get.df$DV, rep(dv, 1)) #check DV
  expect_equal(pc.get.df$EVID, rep(0, 4)) #check EVID
  expect_equal(pc.get.df$MDV, rep(0, 4)) #check MDV
})

funfun = function(){"PC"}
test_that("getDV works with global function in dots",{
  pc.get.df = getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC), DV=PCSTRESN,
                    DOMAIN=funfun())
  #EVID gets defaulted to 0
  #MDV gets defaulted to 0
  expect_equal(names(pc.get.df), c("ID","TIME","DV","EVID","MDV","DOMAIN")) #check column names
  expect_equal(pc.get.df$ID, rep(1:3, each=4)) #check ID
  expect_equal(pc.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(pc.get.df$DV, rep(dv, 3)) #check DV
  expect_equal(pc.get.df$EVID, rep(0, 12)) #check EVID
  expect_equal(pc.get.df$MDV, rep(0, 12)) #check MDV
  expect_equal(pc.get.df$DOMAIN, rep("PC", 12)) #check DOMAIN
})

test_that("getDV works with extra column",{
  pc.get.df = getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC), DV=PCSTRESN, VISIT=VISIT)
  expect_true("VISIT" %in% names(pc.get.df))
  expect_equal(pc.get.df$VISIT,rep("TREATMENT DAY 1",12)) #check VISIT
})

test_that("getDV works with extra column as constant",{
  pc.get.df = getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC), DV=PCSTRESN, DOMAIN="PC")
  expect_true("DOMAIN" %in% names(pc.get.df))
  expect_equal(pc.get.df$DOMAIN,rep("PC",12)) #check DOMAIN
})

test_that("getDV works without ID provided errors",{
  expect_error( getDV(pc.df,TIME=parsedate::parse_date(PCDTC), DV=PCSTRESN, DOMAIN="PC"))
})

test_that("getDV works without TIME provided errors",{
  expect_error( getDV(pc.df,ID=USUBJID, DV=PCSTRESN, DOMAIN="PC"))
})

test_that("getDV works without DV provided errors",{
  expect_error( getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC), DOMAIN="PC"))
})

test_that("getDV replaces BQL with LLOQ if both provided",{
  pc.get.df = getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC),
                    DV=PCSTRESN, BQL=PCSTRESC=="BQL",LLOQ=0.01)
  #get the BQL rows and check DL==0.01
  pc.get.df = subset(pc.get.df,BQL==T)
  expect_equal(pc.get.df$DV,rep(0.01,3))
})

test_that("getDV fails with non-logical BQL with LLOQ if both provided",{
  expect_error(getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC),
                     DV=PCSTRESN, BQL=PCSTRESC,LLOQ=0.01), "TRUE/FALSE")

})

test_that("getDV fails with non-numeric LLOQ when given BQL",{
  expect_error(getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC),
                     DV=PCSTRESN, BQL=PCSTRESC=="BQL",LLOQ="0.01"),
               "non-numeric")

})

test_that("getDV fails with NA LLOQ when given BQL",{
  expect_error(getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC),
                     DV=PCSTRESN, BQL=PCSTRESC=="BQL",LLOQ=NA),
               "non-numeric")
})

test_that("getDV errors with bad mapping",{
  expect_error(getDV(pc.df,ID=USUBJID,TIME=parsedate::parse_date(PCDTC),
                     DV=PCSTRESN, BQL=PCSTRESC=="BQL",LLOQ=arglebargle(EXSTDTC)),"*Error in column*")
})

## Test getting covariates

sample.times = c(-7*24,1) # to have screening and mid-sampling values
visit=c("SCREENING","TREATMENT DAY 1")

wts = c(71,59,88)
age = c(33,27,43)
race= c("WHITE","ASIAN","WHITE")
# fill in the ones that don't change first
dm.df = data.frame(USUBJID=1:3, WT=wts, AGE=age, RACE=race, VISIT="SCREENING")

# time varying CREAT, BILI
creat = c(1,2)
bili = c(1,2)

dtc.df = merge(data.frame(ID=1:3), data.frame(times=sample.times, visit=visit))
lb.df = merge(dtc.df,data.frame(ID=1:3,date=pos.dosedays))
lb.df = cbind(lb.df, CREAT=creat, BILI=bili)
lb.df = transmute(lb.df, USUBJID=ID, LBDTC=parsedate::format_iso_8601(as.character(date+times*3600)),
                  VISIT=visit, CREAT=CREAT, BILI=BILI)
#knock the tz info off the date
lb.df$LBDTC = gsub("\\+00:00","",lb.df$LBDTC)
times = lb.df$LBDTC
#stack observations in lbs
lbs.df = tidyr::gather(lb.df, LBTESTCD, LBSTRESN, CREAT:BILI)

### run tests

test_that("getCovT works with typically formatted data",{
  covt.get.df = getCovT(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD,
                        BILI=BILI, CREAT=CREAT)
  #EVID gets defaulted to 2
  expect_equal(names(covt.get.df), c("ID","TIME","EVID","BILI", "CREAT")) #check column names
  expect_equal(covt.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(covt.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(covt.get.df$BILI, rep(bili, 3)) #check BILI
  expect_equal(covt.get.df$CREAT, rep(creat, 3)) #check CREAT
  expect_equal(covt.get.df$EVID, rep(2, 6)) #check EVID
})

test_that("getCovT works with extra column",{
  covt.get.df = getCovT(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD,
                        VISIT=VISIT, BILI=BILI, CREAT=CREAT)
  #EVID gets defaulted to 2
  expect_equal(names(covt.get.df), c("ID","TIME","EVID","VISIT","BILI", "CREAT")) #check column names
  expect_equal(covt.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(covt.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(covt.get.df$BILI, rep(bili, 3)) #check BILI
  expect_equal(covt.get.df$CREAT, rep(creat, 3)) #check CREAT
  expect_equal(covt.get.df$VISIT, rep(visit, 3)) #check VISIT
  expect_equal(covt.get.df$EVID, rep(2, 6)) #check EVID
})

test_that("getCovT works with extra column, default summarise",{
  covt.get.df = getCovT(lbs.df %>% mutate(LBDTC=first(LBDTC)),ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD,
                        VISIT=VISIT, BILI=BILI, CREAT=CREAT)
  #EVID gets defaulted to 2
  expect_equal(names(covt.get.df), c("ID","TIME","EVID","VISIT","BILI", "CREAT")) #check column names
  expect_equal(covt.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(covt.get.df$TIME, rep(parsedate::parse_date(times)[1],3)) #check TIME
  expect_equal(covt.get.df$BILI, rep(1, 3)) #check BILI
  expect_equal(covt.get.df$CREAT, rep(1, 3)) #check CREAT
  expect_equal(covt.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
  expect_equal(covt.get.df$EVID, rep(2, 3)) #check EVID
})

test_that("getCovT works with extra column, override summarise list",{
  covt.get.df = getCovT(lbs.df %>% mutate(LBDTC=first(LBDTC)),ID=USUBJID,TIME=parsedate::parse_date(LBDTC),
                        covT.val=LBSTRESN, covT.col=LBTESTCD,
                        VISIT=VISIT, BILI=BILI, CREAT=CREAT,
                        fun.summary = list(CREAT="mean",BILI="median"))
  #EVID gets defaulted to 2
  expect_equal(names(covt.get.df), c("ID","TIME","EVID","VISIT","BILI", "CREAT")) #check column names
  expect_equal(covt.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(covt.get.df$TIME, rep(parsedate::parse_date(times[1]),3)) #check TIME
  expect_equal(covt.get.df$BILI, rep(1.5, 3)) #check BILI
  expect_equal(covt.get.df$CREAT, rep(1.5, 3)) #check CREAT
  expect_equal(covt.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
  expect_equal(covt.get.df$EVID, rep(2, 3)) #check EVID
})

test_that("getCovT works with extra column, override summarise vector",{
  covt.get.df = getCovT(lbs.df %>% mutate(LBDTC=first(LBDTC)),ID=USUBJID,TIME=parsedate::parse_date(LBDTC),
                        covT.val=LBSTRESN, covT.col=LBTESTCD,
                        VISIT=VISIT, BILI=BILI, CREAT=CREAT,
                        fun.summary = c(CREAT="mean",BILI="median"))
  #EVID gets defaulted to 2
  expect_equal(names(covt.get.df), c("ID","TIME","EVID","VISIT","BILI", "CREAT")) #check column names
  expect_equal(covt.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(covt.get.df$TIME, rep(parsedate::parse_date(times[1]),3)) #check TIME
  expect_equal(covt.get.df$BILI, rep(1.5, 3)) #check BILI
  expect_equal(covt.get.df$CREAT, rep(1.5, 3)) #check CREAT
  expect_equal(covt.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
  expect_equal(covt.get.df$EVID, rep(2, 3)) #check EVID
})

test_that("getCovT works with extra column, replace single summary",{
  covt.get.df = getCovT(lbs.df %>% mutate(LBDTC=first(LBDTC)),ID=USUBJID,TIME=parsedate::parse_date(LBDTC),
                        covT.val=LBSTRESN, covT.col=LBTESTCD,
                        VISIT=VISIT, BILI=BILI, CREAT=CREAT,
                        fun.summary = "median")
  #EVID gets defaulted to 2
  expect_equal(names(covt.get.df), c("ID","TIME","EVID","VISIT","BILI", "CREAT")) #check column names
  expect_equal(covt.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(covt.get.df$TIME, rep(parsedate::parse_date(times[1]),3)) #check TIME
  expect_equal(covt.get.df$BILI, rep(1.5, 3)) #check BILI
  expect_equal(covt.get.df$CREAT, rep(1.5, 3)) #check CREAT
  expect_equal(covt.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
  expect_equal(covt.get.df$EVID, rep(2, 3)) #check EVID
})


test_that("getCovT works with filter",{
  covt.get.df = getCovT(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD,
                        VISIT=VISIT, BILI=BILI, CREAT=CREAT, covT.filter=VISIT=="SCREENING")
  #EVID gets defaulted to 2
  # only the screening values
  expect_equal(names(covt.get.df), c("ID","TIME","EVID","VISIT","BILI", "CREAT")) #check column names
  expect_equal(covt.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(covt.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(covt.get.df$BILI, rep(bili[1], 3)) #check BILI
  expect_equal(covt.get.df$CREAT, rep(creat[1], 3)) #check CREAT
  expect_equal(covt.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
  expect_equal(covt.get.df$EVID, rep(2, 3)) #check EVID
})

test_that("getCovT warns with zero row filter",{
  expect_warning(covt.get.df <-  getCovT(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD,
                                                    VISIT=VISIT, BILI=BILI, CREAT=CREAT, covT.filter=VISIT=="SCREAMING"))
  expect_equal(covt.get.df,data_frame())
})

test_that("getCovT errors with bad row filter",{
  expect_error(pc.get.df <- covt.get.df <-  getCovT(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD,
                                                    VISIT=VISIT, BILI=BILI, CREAT=CREAT, covT.filter=arglebargle(USUBJID)), "*unable to process covT.filter*")
})

test_that("getCovT processes key columns only once",{
  covt.get.df = getCovT(lb.df %>% rename(ID=USUBJID),ID=ID+1, TIME=parsedate::parse_date(LBDTC), BILI=BILI, CREAT=CREAT,
                      VISIT=paste0(VISIT,1))
  expect_equal(covt.get.df$VISIT, rep(c("SCREENING1","TREATMENT DAY 11"), 3)) #check VISIT
  expect_equal(covt.get.df$ID, rep(c(1:3+1), each=2)) #check VISIT
})

globaltext="SCREENING"
filtfun = function(){globaltext}
test_that("getCovT works with global function in filter",{
  covt.get.df = getCovT(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD,
                        VISIT=VISIT, BILI=BILI, CREAT=CREAT, covT.filter=VISIT==filtfun())
  #EVID gets defaulted to 2
  # only the screening values
  expect_equal(names(covt.get.df), c("ID","TIME","EVID","VISIT","BILI", "CREAT")) #check column names
  expect_equal(covt.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(covt.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(covt.get.df$BILI, rep(bili[1], 3)) #check BILI
  expect_equal(covt.get.df$CREAT, rep(creat[1], 3)) #check CREAT
  expect_equal(covt.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
  expect_equal(covt.get.df$EVID, rep(2, 3)) #check EVID
})

funfun = function(){"LB"}
test_that("getCovT works with global function in ...",{
  covt.get.df = getCovT(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD,
                        VISIT=VISIT, BILI=BILI, CREAT=CREAT, covT.filter=VISIT==filtfun(),
                        DOMAIN=funfun())
  #EVID gets defaulted to 2
  # only the screening values
  expect_equal(names(covt.get.df), c("ID","TIME","EVID","VISIT","BILI", "CREAT", "DOMAIN")) #check column names
  expect_equal(covt.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(covt.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(covt.get.df$BILI, rep(bili[1], 3)) #check BILI
  expect_equal(covt.get.df$CREAT, rep(creat[1], 3)) #check CREAT
  expect_equal(covt.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
  expect_equal(covt.get.df$EVID, rep(2, 3)) #check EVID
  expect_equal(covt.get.df$DOMAIN, rep("LB", 3)) #check EVID
})

test_that("getCovT fails with missing ID",{
  expect_error(getCovT(lbs.df,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD))
})

test_that("getCovT fails with missing TIME",{
  expect_error(getCovT(lbs.df,ID=USUBJID, covT.val=LBSTRESN, covT.col=LBTESTCD))
})

test_that("getCovT fails with missing covT.val",{
  expect_error(getCovT(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.col=LBTESTCD))
})
test_that("getCovT fails with missing covT.col",{
  expect_error(getCovT(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN))
})

test_that("getCovT errors with bad mapping",{
  expect_error(getCovT(lbs.df,ID=USUBJID,TIME=arglebargle(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD),"*Error in column*")
})

test_that("getCovT works with long data with extra column  and units ",{
  lbsu.df = lbs.df %>% mutate(LBSTRESU="mg/L")
  covT.get.df = getCovT(lbsu.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), covT.val=LBSTRESN, covT.col=LBTESTCD,
                      VISIT=VISIT,BILI=BILI, CREAT=CREAT, fun.summary="mean",Units=LBSTRESU)
  expect_equal(names(covT.get.df), c("ID","TIME", "EVID","VISIT","BILI", "CREAT")) #check column names
  expect_equal(covT.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(covT.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(as.numeric(covT.get.df$BILI), rep(1:2,3)) #check BILI
  expect_equal(as.numeric(covT.get.df$CREAT), rep(1:2,3)) #check CREAT
  expect_equal(covT.get.df$VISIT, rep(c("SCREENING","TREATMENT DAY 1"),3)) #check VISIT
  expect_equal(unit_cols(covT.get.df),c(BILI="mg/L",CREAT="mg/L"))
})

test_that("getCovT works with long data with extra column, summary override, multiple units ",{
  lbsu.df = lbs.df %>% mutate(LBSTRESU="mg/L")
  lbsu.df$LBSTRESU[2] = 'mol/L'
  expect_warning(covT.get.df <- getCovT(lbsu.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC),
                                        covT.val=LBSTRESN, covT.col=LBTESTCD,
                                        VISIT=VISIT,BILI=BILI, CREAT=CREAT,
                                        fun.summary="mean",Units=LBSTRESU),
                 "*multiple units*")
  expect_equal(names(covT.get.df), c("ID","TIME", "EVID","VISIT","BILI", "CREAT")) #check column names
  expect_equal(covT.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(covT.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(as.numeric(covT.get.df$BILI), rep(1:2,3)) #check BILI
  expect_equal(as.numeric(covT.get.df$CREAT), rep(1:2,3)) #check CREAT
  expect_equal(covT.get.df$VISIT, rep(c("SCREENING","TREATMENT DAY 1"),3)) #check VISIT
  expect_equal(unit_cols(covT.get.df),c(BILI="mg/L",CREAT="mg/L"))
})

## test getCov

test_that("getCov works with long formatted data",{
  cov.get.df = getCov(lbs.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                       cov.keys=c("ID","VISIT"), BILI=BILI, CREAT=CREAT, VISIT=VISIT)
  expect_equal(names(cov.get.df), c("ID","VISIT","TIME","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(cov.get.df$BILI, rep(bili, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat, 3)) #check CREAT

})

test_that("getCov works with long data with extra column",{
  cov.get.df = getCov(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                       VISIT=VISIT, cov.keys=c("ID","VISIT"), BILI=BILI, CREAT=CREAT)
  expect_equal(names(cov.get.df), c("ID","VISIT","TIME","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(cov.get.df$BILI, rep(bili, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat, 3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep(visit, 3)) #check VISIT
})

test_that("getCov works with long data with extra column and summary func default",{
  cov.get.df = getCov(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                      VISIT=VISIT, cov.keys=c("ID"), BILI=BILI, CREAT=CREAT)
  expect_equal(names(cov.get.df), c("ID","TIME","VISIT","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(cov.get.df$BILI, rep(1,3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(1,3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING",3)) #check VISIT
})

test_that("getCov works with long data with extra column and summary override ",{
  cov.get.df = getCov(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                      VISIT=VISIT, cov.keys=c("ID"), BILI=BILI, CREAT=CREAT, fun.summary="mean")
  expect_equal(names(cov.get.df), c("ID","TIME","VISIT","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(cov.get.df$BILI, rep(1.5,3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(1.5,3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING",3)) #check VISIT
})

test_that("getCov works with long data with extra column, summary override and units ",{
  lbsu.df = lbs.df %>% mutate(LBSTRESU="mg/L")
  cov.get.df = getCov(lbsu.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                      VISIT=VISIT, cov.keys=c("ID"), BILI=BILI, CREAT=CREAT, fun.summary="mean",Units=LBSTRESU)
  expect_equal(names(cov.get.df), c("ID","TIME","VISIT","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(as.numeric(cov.get.df$BILI), rep(1.5,3)) #check BILI
  expect_equal(as.numeric(cov.get.df$CREAT), rep(1.5,3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING",3)) #check VISIT
  expect_equal(unit_cols(cov.get.df),c(BILI="mg/L",CREAT="mg/L"))
})

test_that("getCov works with long data with extra column, summary override, multiple units ",{
  lbsu.df = lbs.df %>% mutate(LBSTRESU="mg/L")
  lbsu.df$LBSTRESU[2] = 'mol/L'
  expect_warning(cov.get.df <- getCov(lbsu.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC),
                                     cov.val=LBSTRESN, cov.col=LBTESTCD,
                                     VISIT=VISIT, cov.keys=c("ID"), BILI=BILI, CREAT=CREAT,
                                     fun.summary="mean",Units=LBSTRESU),
                 "*multiple units*")
  expect_equal(names(cov.get.df), c("ID","TIME","VISIT","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(as.numeric(cov.get.df$BILI), rep(1.5,3)) #check BILI
  expect_equal(as.numeric(cov.get.df$CREAT), rep(1.5,3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING",3)) #check VISIT
  expect_equal(unit_cols(cov.get.df),c(BILI="mg/L",CREAT="mg/L"))
})

test_that("getCov works with long data with extra column and individual summary override ",{
  cov.get.df = getCov(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                      VISIT=VISIT, cov.keys=c("ID"), BILI=BILI, CREAT=CREAT, fun.summary=c(BILI="mean"))
  expect_equal(names(cov.get.df), c("ID","TIME","VISIT","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(cov.get.df$BILI, rep(1.5,3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(1,3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING",3)) #check VISIT
})

test_that("getCov works with long data with extra column and individual summary override 2",{
  cov.get.df = getCov(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                      VISIT=VISIT, cov.keys=c("ID"), BILI=BILI, CREAT=CREAT, fun.summary=c(BILI="mean", CREAT="median"))
  expect_equal(names(cov.get.df), c("ID","TIME","VISIT","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(cov.get.df$BILI, rep(1.5,3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(1.5,3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING",3)) #check VISIT
})

test_that("getCov works with long data with extra column and individual summary override 3 (NA data)",{
  lbs1.df = lbs.df
  lbs1.df[c(1,9),"LBSTRESN"] = NA
  lbs1.df[7,"VISIT"]=NA
  cov.get.df = getCov(lbs1.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                      VISIT=VISIT, cov.keys=c("ID"), BILI=BILI, CREAT=CREAT, fun.summary=c(BILI="mean_", CREAT="median_"))
  expect_equal(names(cov.get.df), c("ID","TIME","VISIT","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(cov.get.df$BILI, c(1.5,2,1.5)) #check BILI
  expect_equal(cov.get.df$CREAT, c(2,1.5,1.5)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING",3)) #check VISIT
})

test_that("getCov works with long data with filter",{
  cov.get.df = getCov(lbs.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                       VISIT=VISIT, cov.keys = "ID", cov.filter=VISIT=="SCREENING", BILI=BILI, CREAT=CREAT)
  # only the screening values
  expect_equal(names(cov.get.df), c("ID","TIME","VISIT","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(cov.get.df$BILI, rep(bili[1], 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat[1], 3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
})

test_that("getCov warns with zero row filter",{
  expect_warning(cov.get.df <- getCov(lbs.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                                     VISIT=VISIT, cov.keys = "ID", cov.filter=VISIT=="SCREAMING", BILI=BILI, CREAT=CREAT))
  expect_equal(cov.get.df,data_frame())
})

test_that("getCov errors with bad row filter",{
  expect_error(cov.get.df <-  getCov(lbs.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                                      VISIT=VISIT, cov.keys = "ID", cov.filter=arglbargle(VISIT), BILI=BILI, CREAT=CREAT), "*unable to process cov.filter*")
})

test_that("getCov works with long data with global function as filter",{
  cov.get.df = getCov(lbs.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD,
                      VISIT=VISIT, cov.keys = "ID", cov.filter=VISIT==filtfun(), BILI=BILI, CREAT=CREAT)
  # only the screening values
  expect_equal(names(cov.get.df), c("ID","TIME","VISIT","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(cov.get.df$BILI, rep(bili[1], 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat[1], 3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
})

test_that("getCov works with wide formatted data",{
  cov.get.df = getCov(lb.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=BILI,
                      CREAT=CREAT, cov.keys=c("ID","TIME"))
  expect_equal(names(cov.get.df), c("ID","TIME","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(cov.get.df$BILI, rep(bili, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat, 3)) #check CREAT
})

test_that("getCov works with wide formatted data - summarize same",{
  cov.get.df = getCov(lb.df,ID=USUBJID, BILI=BILI, CREAT=CREAT, cov.keys="ID", fun.summary = "mean")
  expect_equal(names(cov.get.df), c("ID","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, 1:3) #check ID
  expect_equal(cov.get.df$BILI, rep(1.5, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(1.5, 3)) #check CREAT
})

test_that("getCov works with wide formatted data - summarize different",{
  cov.get.df = getCov(lb.df,ID=USUBJID, BILI=BILI, CREAT=CREAT, cov.keys="ID",
                      fun.summary = list(BILI="mean_", CREAT="median_"))
  expect_equal(names(cov.get.df), c("ID","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, 1:3) #check ID
  expect_equal(cov.get.df$BILI, rep(1.5, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(1.5, 3)) #check CREAT
})


test_that("getCov works with wide data with filter",{
  cov.get.df = getCov(lb.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=BILI, CREAT=CREAT,
                       cov.keys="ID", VISIT=VISIT, cov.filter=VISIT=="SCREENING")
  # only the screening values
  expect_equal(names(cov.get.df), c("ID","TIME","BILI", "CREAT", "VISIT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(cov.get.df$BILI, rep(bili[1], 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat[1], 3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
})

test_that("getCov works with wide data with global function as filter",{
  cov.get.df = getCov(lb.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=BILI, CREAT=CREAT,
                      cov.keys="ID", VISIT=VISIT, cov.filter=VISIT==filtfun())
  # only the screening values
  expect_equal(names(cov.get.df), c("ID","TIME","BILI", "CREAT", "VISIT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[c(1,3,5)]) #check TIME
  expect_equal(cov.get.df$BILI, rep(bili[1], 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat[1], 3)) #check CREAT
  expect_equal(cov.get.df$VISIT, rep("SCREENING", 3)) #check VISIT
})

test_that("getCov sets merge keys by grouping",{
  cov.get.df = getCov(lb.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=BILI, CREAT=CREAT,
                       VISIT=VISIT,cov.keys=c("ID","VISIT"))
  expect_equal(as.character(groups(cov.get.df)),c("ID","VISIT"))

})

test_that("getCov processes key columns only once",{
  cov.get.df = getCov(lb.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=BILI, CREAT=CREAT,
                      VISIT=paste0(VISIT,1),cov.keys=c("ID","VISIT"))
  expect_equal(as.character(groups(cov.get.df)),c("ID","VISIT"))
  expect_equal(cov.get.df$VISIT, rep(c("SCREENING1","TREATMENT DAY 11"), 3)) #check VISIT
})

test_that("getCov fails with missing ID",{
  expect_error(getCov(lbs.df,TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN, cov.col=LBTESTCD),
               "getCov cannot have missing argument 'ID'")
})

test_that("getCov fails with missing cov.val and not cov.col",{
  expect_error(getCov(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), cov.col=LBTESTCD),
               "getCov - both or neither of cov.col and cov.val must be specified")
})

test_that("getCov fails with missing cov.col and not cov.val",{
  expect_error(getCov(lbs.df,ID=USUBJID,TIME=parsedate::parse_date(LBDTC), cov.val=LBSTRESN),
               "getCov - both or neither of cov.col and cov.val must be specified")
})

test_that("getCov errors with bad mapping",{
  expect_error(getCov(lb.df,ID=USUBJID, TIME=arglebargle(LBDTC), BILI=BILI, CREAT=CREAT,
                      VISIT=VISIT,cov.keys=c("ID","VISIT")),"*Error in column*")
})

## Test getDomain
test_that("getDomain works without filter",{
  cov.get.df = getDomain(lb.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=BILI, CREAT=CREAT)
  expect_equal(names(cov.get.df), c("ID","TIME","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(cov.get.df$BILI, rep(bili, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat, 3)) #check CREAT
})

test_that("getDomain works with filter",{
  cov.get.df = getDomain(lb.df, .filter=BILI==2, ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=BILI, CREAT=CREAT)
  expect_equal(names(cov.get.df), c("ID","TIME","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[(1:3)*2]) #check TIME
  expect_equal(cov.get.df$BILI, rep(2, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(2, 3)) #check CREAT
})

globaltext=2
filtfun = function(){globaltext}
test_that("getDomain works with global function as filter",{
  cov.get.df = getDomain(lb.df, .filter=BILI==filtfun(), ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=BILI, CREAT=CREAT)
  expect_equal(names(cov.get.df), c("ID","TIME","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=1)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)[(1:3)*2]) #check TIME
  expect_equal(cov.get.df$BILI, rep(2, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(2, 3)) #check CREAT
})

test_that("getDomain ignores missing columns - without filter",{
  cov.get.df = getDomain(lb.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=NOTTHERE, CREAT=CREAT)
  expect_equal(names(cov.get.df), c("ID","TIME", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)) #check TIME
  #expect_equal(cov.get.df$BILI, rep(bili, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat, 3)) #check CREAT
})

test_that("getDomain ignores missing columns in transform - without filter",{
  cov.get.df = getDomain(lb.df,ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=NOTTHERE+BILI, CREAT=CREAT)
  expect_equal(names(cov.get.df), c("ID","TIME", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)) #check TIME
  #expect_equal(cov.get.df$BILI, rep(bili, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat, 3)) #check CREAT
})

test_that("getDomain ignores missing columns - with filter",{
  cov.get.df = getDomain(lb.df, .filter=NOTTHERE==2, ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=BILI, CREAT=CREAT)
  expect_equal(names(cov.get.df), c("ID","TIME","BILI", "CREAT")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(cov.get.df$BILI, rep(bili, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat, 3)) #check CREAT
})

test_that("getDomain processes transformations on renamed columns",{
  cov.get.df = getDomain(lb.df, ID=USUBJID, TIME=parsedate::parse_date(LBDTC), BILI=BILI, CREAT=CREAT, ctime=as.character(TIME))
  expect_equal(names(cov.get.df), c("ID","TIME","BILI", "CREAT","ctime")) #check column names
  expect_equal(cov.get.df$ID, rep(1:3, each=2)) #check ID
  expect_equal(cov.get.df$TIME, parsedate::parse_date(times)) #check TIME
  expect_equal(cov.get.df$BILI, rep(bili, 3)) #check BILI
  expect_equal(cov.get.df$CREAT, rep(creat, 3)) #check CREAT
  expect_equal(cov.get.df$ctime, as.character(parsedate::parse_date(times)))
})
