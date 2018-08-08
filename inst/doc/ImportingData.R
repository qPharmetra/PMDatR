## ------------------------------------------------------------------------
library(PMDatR)
library(parsedate)
library(dplyr)
library(yaml)

## ------------------------------------------------------------------------

ex.dom = domain(list(name="EX", filepath="ex.sas7bdat")) 


DM=list() 
DM$domains = list(EX=ex.dom)

## ------------------------------------------------------------------------
ex.dom = load.domain(ex.dom)

## ------------------------------------------------------------------------
names(ex.dom)

## ------------------------------------------------------------------------
ex.dom$Data

## ------------------------------------------------------------------------
preprocess.EX = function(.dom){
  .dom$Data = .dom$Data %>% mutate(TIME=parse_date(EXSTDTC)) %>% 
    select(USUBJID, TIME, EXSTDTC, EXSEQ, EXDOSE, VISIT, EPOCH)
  .dom
}

ex.dom = load.domain(ex.dom, .fun = preprocess.EX)
ex.dom$Data

## ------------------------------------------------------------------------
ex.dom$outMeta

## ------------------------------------------------------------------------
cat(as.yaml(ex.dom$outMeta))

## ------------------------------------------------------------------------
ex.dom = domain(list(name="EX", filepath="ex.xlsx")) 
ex.dom = load.domain(ex.dom, .fun=preprocess.EX)
ex.dom$Data

## ------------------------------------------------------------------------

ex.dom = domain(list(name="EX", filepath="ex.csv", txtSettings = list(sep=",", header=T))) 
ex.dom = load.domain(ex.dom, .fun=preprocess.EX)
ex.dom$Data

