# This file would normally be called DMfunctions.R, and would be automatically created by DM initialization.
# Here, the functions are spoofed to show how they would be assembled from some assumed settings.  These are low
# level functions for the merge process.
#library(parsedate)
library(dplyr)


### Load and Preprocess domains

preprocess.domains = function(DMobj){
  DMobj$domains$PC = load.domain(DMobj$domains$PC, .fun=preprocess.PC)
  DMobj$domains$EX = load.domain(DMobj$domains$EX, .fun=preprocess.EX)
  DMobj$domains$DM = load.domain(DMobj$domains$DM, .fun=preprocess.DM)
  DMobj$domains$LB = load.domain(DMobj$domains$LB, .fun=preprocess.LB)
  DMobj
}

## pre-processing functions apply filters, transformations, pre-merge specified by settings.
  ##  Set data in dom$Data, and return dom

preprocess.PC = function(dom){
  dom
}

preprocess.EX = function(dom){
  dom
}

preprocess.DM = function(dom){
  dom
}

preprocess.LB = function(dom){
  dom
}

### process data
process.EX = function(DM)
{
  # dosing set #1

  ex1.df = getIndividualDoses(DM$domains$EX$Data,
                              ID=USUBJID,
                              TIME=parsedate::parse_date(EXSTDTC),
                              AMT=EXDOSE,
                              TRT=EXTRT,
                              EPOCH=EPOCH,
                              NTIME=0
                              )

  bind_rows(ex1.df)

}

process.DV = function(DM){
  # DV #1
  dv1.df = getDV(DM$domains$PC$Data,
                 ID=USUBJID,
                 TIME=parsedate::parse_date(PCDTC),
                 DV=PCSTRESN,
                 BQL=PCORRES=='BQL',
                 LLOQ=PCLLOQ,
                 MDV=PCSTAT=="NOT DONE",
                 NTIME=PCTPTNUM,
                 PCSTAT=PCSTAT,
                 EPOCH=EPOCH,
                 dv.filter=PCTEST=="ANALYTE1"
                 )
  # combine all DVs
  bind_rows(dv1.df)

}


process.Cov = function(DM){
  # demographics covariates
  cov1.df = getCov(DM$domains$DM$Data,
                   ID = USUBJID,
                   AGE=AGE,
                   SEX=SEX,
                   RACE=RACE,
                   ETHNIC=ETHNIC,
                   cov.keys = c("ID")
                   )

  # LB covariates
  cov2.df = getCov(DM$domains$LB$Data,
                   ID = USUBJID,
                   cov.filter = LBBLFL=="Y",
                   cov.col = LBTESTCD,
                   cov.val = LBSTRESN,
                   EPOCH=EPOCH,
                   cov.keys = c("ID", "EPOCH"),
                   CREAT=CREAT
                   )
  list(cov1.df, cov2.df)

  # use the default keys for the domain unless overridden here
}

process.CovT = function(DM){
  # CovT set #1
  cols = c()
  id="USUBJID"
  time="parsedate::parse_date(EXSTDTC)"

  covt1.df = getCovT(DM$domains$LB$Data,
                     ID = USUBJID,
                     TIME = parsedate::parse_date(LBDTC),
                     covT.filter = EPOCH !="Screening",
                     covT.col = LBTESTCD,
                     covT.val = LBSTRESN,
                     EPOCH=EPOCH,
                     AST=AST,
                     ALT=ALT,
                     ALB=ALB,
                     BILI=BILI
                     )

  bind_rows(covt1.df)
}

## pre-merge hook
pre.merge.hook = function(){
  # custom user code goes here, which can operate using .GlobalEnv variable definitions
  # access domain data with DM$domains$<domain name>$Data


  # set the values in the global context
  # EX.df <<- EX.df

}


## post-merge hook
post.merge.hook = function() {
  # custom user code goes here, which can operate using .GlobalEnv variable definitions
  # access merged dataset with database.df, pre-merge data with DM$domains$<domain name>$Data

  # change M/F to MALE/FEMALE, get rid of NA in PCSTAT
  .data = parent.frame()$database.df #needed for testthat, which does strange things with environments

  # set the values in the global context
   database.df <<- .data %>% mutate(SEX=conditional_values(MALE=SEX=="M",default="FEMALE"),
                                          PCSTAT=ifelse(is.na(PCSTAT),"",PCSTAT))
}


## post merge transform
## These functions will work with the merged dataset, passed in as .data.  Return the modified dataset.

post.transform = function(.data){
  # apply interpolation rules for CovT columns
  .data %>% mutate_each(funs(locf),ALB,AST,ALT,BILI,CREAT)
}

post.filter = function(.data){
  # remove a data point
  .data %>% filter(RECID!=27)
}

apply.exclusions = function(.data){
  # apply exclusions
  ## EXCLUSION either OK or BQL, then add flags
  .data %>% mutate(EXCL=conditional_values(BQL=BQL==T & EVID==0, ND=PCSTAT=="NOT DONE", default="OK")) %>%
    mutate(BQL.fl=BQL==T & EVID==0, ND.fl=PCSTAT=="NOT DONE")
}
