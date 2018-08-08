## ------------------------------------------------------------------------
library(PMDatR)
library(dplyr)

#DM = DataManagement(settings.path) would generate the DM object that we construct here

# these lists come from the settings.path YAML file
pc.settings = domain(list(name="PC",filepath="pc.sas7bdat"))
ex.settings = domain(list(name="EX",filepath="ex.sas7bdat"))
dm.settings = domain(list(name="DM",filepath="dm.sas7bdat"))
lb.settings = domain(list(name="LB",filepath="lb.sas7bdat"))

DM=list()
DM$domains = list(PC=pc.settings, EX=ex.settings, DM=dm.settings, LB=lb.settings)

## ------------------------------------------------------------------------
DM

## ------------------------------------------------------------------------
source("DMfunctions_spoofed.R")

## ----echo=F--------------------------------------------------------------
# This is just to display the definitions of the dynamic functions
library(knitr)

# Define insert_fun.
insert_fun = function(name) {
  read_chunk(lines = capture.output(dump(name, '')), labels = paste(name, 'source', sep = '-'))
}

insert_fun("preprocess.domains") # display in chunck labelled "preprocess.domains-source"
insert_fun("preprocess.DM") # display in chunck labelled "preprocess.DM-source"
insert_fun("preprocess.EX") # display in chunck labelled "preprocess.EX-source"
insert_fun("preprocess.LB") # display in chunck labelled "preprocess.LB-source"
insert_fun("preprocess.PC") # display in chunck labelled "preprocess.PC-source"
insert_fun("process.EX") # display in chunck labelled "process.EX-source"
insert_fun("process.DV") # display in chunck labelled "process.DV-source"
insert_fun("process.Cov") # display in chunck labelled "process.Cov-source"
insert_fun("process.CovT") # display in chunck labelled "process.CovT-source"
insert_fun("pre.merge.hook") # display in chunck labelled "pre.merge.hook-source"
insert_fun("post.merge.hook") # display in chunck labelled "post.merge.hook-source"
insert_fun("post.transform") # display in chunck labelled "post.transform-source"
insert_fun("post.filter") # display in chunck labelled "post.filter-source"
insert_fun("apply.exclusions") # display in chunck labelled "apply.exclusions-source"

## ----preprocess.domains-source, eval=F-----------------------------------
#  preprocess.domains <-
#  function(DMobj){
#    DMobj$domains$PC = load.domain(DMobj$domains$PC, .fun=preprocess.PC)
#    DMobj$domains$EX = load.domain(DMobj$domains$EX, .fun=preprocess.EX)
#    DMobj$domains$DM = load.domain(DMobj$domains$DM, .fun=preprocess.DM)
#    DMobj$domains$LB = load.domain(DMobj$domains$LB, .fun=preprocess.LB)
#    DMobj
#  }

## ----preprocess.EX-source, eval=F----------------------------------------
#  preprocess.EX <-
#  function(dom){
#    dom
#  }

## ----preprocess.PC-source, eval=F----------------------------------------
#  preprocess.PC <-
#  function(dom){
#    dom
#  }

## ----preprocess.DM-source, eval=F----------------------------------------
#  preprocess.DM <-
#  function(dom){
#    dom
#  }

## ----preprocess.LB-source, eval=F----------------------------------------
#  preprocess.LB <-
#  function(dom){
#    dom
#  }

## ------------------------------------------------------------------------
DM = preprocess.domains(DM)

## ----process.EX-source, eval=F-------------------------------------------
#  process.EX <-
#  function(DM)
#  {
#    # dosing set #1
#  
#    ex1.df = getIndividualDoses(DM$domains$EX$Data,
#                                ID=USUBJID,
#                                TIME=parsedate::parse_date(EXSTDTC),
#                                AMT=EXDOSE,
#                                TRT=EXTRT,
#                                EPOCH=EPOCH,
#                                NTIME=0
#                                )
#  
#    bind_rows(ex1.df)
#  
#  }

## ------------------------------------------------------------------------
EX.df = process.EX(DM)

## ------------------------------------------------------------------------
glimpse(EX.df)

## ----process.DV-source, eval=F-------------------------------------------
#  process.DV <-
#  function(DM){
#    # DV #1
#    dv1.df = getDV(DM$domains$PC$Data,
#                   ID=USUBJID,
#                   TIME=parsedate::parse_date(PCDTC),
#                   DV=PCSTRESN,
#                   BQL=PCORRES=='BQL',
#                   LLOQ=PCLLOQ,
#                   MDV=PCSTAT=="NOT DONE",
#                   NTIME=PCTPTNUM,
#                   PCSTAT=PCSTAT,
#                   EPOCH=EPOCH,
#                   dv.filter=PCTEST=="ANALYTE1"
#                   )
#    # combine all DVs
#    bind_rows(dv1.df)
#  
#  }

## ------------------------------------------------------------------------
DV.df = process.DV(DM)

## ------------------------------------------------------------------------
glimpse(DV.df)

## ----process.Cov-source, eval=F------------------------------------------
#  process.Cov <-
#  function(DM){
#    # demographics covariates
#    cov1.df = getCov(DM$domains$DM$Data,
#                     ID = USUBJID,
#                     AGE=AGE,
#                     SEX=SEX,
#                     RACE=RACE,
#                     ETHNIC=ETHNIC,
#                     cov.keys = c("ID")
#                     )
#  
#    # LB covariates
#    cov2.df = getCov(DM$domains$LB$Data,
#                     ID = USUBJID,
#                     cov.filter = LBTESTCD=="CREAT" & LBBLFL=="Y",
#                     cov.col = LBTESTCD,
#                     cov.val = LBSTRESN,
#                     EPOCH=EPOCH,
#                     cov.keys = c("ID", "EPOCH")
#                     )
#    list(cov1.df, cov2.df)
#  
#    # use the default keys for the domain unless overridden here
#  }

## ------------------------------------------------------------------------
Cov.l = process.Cov(DM)

## ------------------------------------------------------------------------
glimpse(Cov.l[[1]])
glimpse(Cov.l[[2]])

## ----process.CovT-source, eval=F-----------------------------------------
#  process.CovT <-
#  function(DM){
#    # CovT set #1
#    cols = c()
#    id="USUBJID"
#    time="parsedate::parse_date(EXSTDTC)"
#  
#    covt1.df = getCovT(DM$domains$LB$Data,
#                       ID = USUBJID,
#                       TIME = parsedate::parse_date(LBDTC),
#                       covT.filter = LBTESTCD %in% c("AST","ALT","ALB","BILI") & EPOCH !="Screening",
#                       covT.col = LBTESTCD,
#                       covT.val = LBSTRESN,
#                       EPOCH=EPOCH
#                       )
#  
#    bind_rows(covt1.df)
#  }

## ------------------------------------------------------------------------
CovT.df = process.CovT(DM)

## ------------------------------------------------------------------------
glimpse(CovT.df)

## ----pre.merge.hook-source, eval=F---------------------------------------
#  pre.merge.hook <-
#  function(){
#    # custom user code goes here, which can operate using .GlobalEnv variable definitions
#    # access domain data with DM$domains$<domain name>$Data
#  
#  
#    # set the values in the global context
#    # EX.df <<- EX.df
#  
#  }

## ------------------------------------------------------------------------
pre.merge.hook()

## ------------------------------------------------------------------------
events.df = append.events(EX.df, DV.df)

## ------------------------------------------------------------------------
events.df = append.CovT(events.df, CovT.df)

## ------------------------------------------------------------------------
database.df = merge.Cov(events.df, Cov.l)

## ----post.merge.hook-source, eval=F--------------------------------------
#  post.merge.hook <-
#  function() {
#    # custom user code goes here, which can operate using .GlobalEnv variable definitions
#    # access merged dataset with database.df, pre-merge data with DM$domains$<domain name>$Data
#  
#    # change M/F to MALE/FEMALE, get rid of NA in PCSTAT
#    .data = parent.frame()$database.df #needed for testthat, which does strange things with environments
#  
#    # set the values in the global context
#     database.df <<- .data %>% mutate(SEX=conditionalValues(MALE=SEX=="M",default="FEMALE"),
#                                            PCSTAT=ifelse(is.na(PCSTAT),"",PCSTAT))
#  }

## ------------------------------------------------------------------------
post.merge.hook()

## ----post.transform-source, eval=F---------------------------------------
#  post.transform <-
#  function(.data){
#    # apply interpolation rules for CovT columns
#    .data %>% mutate_each(funs(locf),ALB,AST,ALT,BILI,CREAT)
#  }

## ----post.filter-source, eval=F------------------------------------------
#  post.filter <-
#  function(.data){
#    # remove a data point
#    .data %>% filter(RECID!=27)
#  }

## ----apply.exclusions-source, eval=F-------------------------------------
#  apply.exclusions <-
#  function(.data){
#    # apply exclusions
#    ## EXCLUSION either OK or BQL, then add flags
#    .data %>% mutate(EXCL=conditionalValues(BQL=BQL==T & EVID==0, ND=PCSTAT=="NOT DONE", default="OK")) %>%
#      mutate(BQL.fl=BQL==T & EVID==0, ND.fl=PCSTAT=="NOT DONE")
#  }

## ------------------------------------------------------------------------
database.df = post.merge.refactoring(database.df, fun.transform = post.transform, fun.filter = post.filter,
                                     fun.exclude = apply.exclusions)

## ----width=12------------------------------------------------------------
kable(database.df %>% filter(ID=="STUDY-01-1"))

