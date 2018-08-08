# this is a unit test that mimics a dynamically created PMDatR data script, but just for loading a single domain

### Header section
library(testthat)
library(PMDatR)

context("Test pre-merge for a domain")
path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

### Setup: this section would normally be setup by the PMDatR::DataManagement functions
#DM = DataManagement(settings.path) would generate the DM object that we construct here
test_that("Pre-merge can be done with another domain", {
# these lists come from the settings.path YAML file
dm.dom = domain(list(name="DM",filepath=file.path(path.testroot,"testdata/data1/csv/dm.csv"),
                     FileSettings=list(sep=",", header=T)))
tte.dom = domain(list(name="TTE",filepath=file.path(path.testroot,"testdata/data1/csv/adtte.csv"),
                      FileSettings=list(sep=",", header=T)))

#pre-merge settings from GUI, can specify any loaded domain
dm.dom$premerge[[1]] = list(domain=tte.dom, jointype="left", keys="SUBJID", cols=list(CENSOR="CNSR", TTE="AVAL"))

DM=list()
DM$domains = list(DM=dm.dom, TTE=tte.dom)

###

## DM generated functions

preprocess.domains = function(DMobj){
  DMobj$domains$DM = load.domain(DMobj$domains$DM, .fun=preprocess.DM)
  DMobj$domains$TTE =load.domain(DMobj$domains$TTE)
  DMobj
}

## Script functions

## pre-processing functions apply filters, transformations, pre-merge specified by settings.
  ## set data in dom$Data and return dom

preprocess.DM = function(dom){
  # a premerge due to settings (not shown in this example)
  # this call is written by the DM object based on the settings given above
  dom$Data = pre.merge(dom, dom$premerge[[1]]$domain, jointype = "left", keys="SUBJID", CENSOR=CNSR, TTE=AVAL)
  dom
}

## Pre-process domains (load, filter, pre-merge etc)
DM = preprocess.domains(DM)

DM$domains$DM$outMeta
test.df = select(DM$domains$DM$Data, USUBJID, SUBJID, AGE, TTE, CENSOR)

## put in some checks
#load the tte file directly
tte.df = read.csv(file.path(path.testroot,"testdata/data1/csv/adtte.csv"))
dm.df = read.csv(file.path(path.testroot,"testdata/data1/csv/dm.csv"))

# did we get the desired data frame?
expect_equal(names(test.df), c("USUBJID","SUBJID","AGE","TTE","CENSOR")) #check column names
expect_equal(test.df$USUBJID, paste0("STUDY-01-",1:10)) #check USUBJID, comes from DM
expect_equal(test.df$SUBJID, tte.df$SUBJID) # check SUBJID, in both DM and TTE
expect_equal(test.df$TTE, tte.df$AVAL)  #check TTE
expect_equal(test.df$CENSOR, tte.df$CNSR) # check CENSOR
expect_equal(test.df$AGE, dm.df$AGE)
})
