# this is a unit test that mimics a dynamically created PMDatR data script

### Header section
library(testthat)
library(PMDatR)


path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

### Setup: this section would normally be setup by the PMDatR::DataManagement functions
#DM = DataManagement(settings.path) would generate the DM object that we construct here

# these lists come from the settings.path YAML file
pc.settings = domain(list(name="PC",filepath=file.path(path.testroot,"/testdata/data1/sas/pc.sas7bdat")))
ex.settings = domain(list(name="EX",filepath=file.path(path.testroot,"/testdata/data1/sas/ex.sas7bdat")))
dm.settings = domain(list(name="DM",filepath=file.path(path.testroot,"/testdata/data1/sas/dm.sas7bdat")))
lb.settings = domain(list(name="LB",filepath=file.path(path.testroot,"/testdata/data1/sas/lb.sas7bdat")))

DM=list()
DM$domains = list(PC=pc.settings, EX=ex.settings, DM=dm.settings, LB=lb.settings)
DM
###

## Merge script

### Source the file created by DM object initialization
source(file.path(path.testroot,"testthat/DMfunctions_spoofed.R"))

## Pre-process domains (load, filter, pre-merge etc)
DM = preprocess.domains(DM)

## Process Dosing
EX.df = process.EX(DM)

## Process DV items

DV.df = process.DV(DM)

## Process Covariate items

Cov.l = process.Cov(DM)

## Process Time-Varying Covariate items (CovT)

CovT.df = process.CovT(DM)

## Ready for Merging

## Check for errors and report

### PRE-MERGE HOOK GOES HERE

pre.merge.hook()

### Append events

events.df = append.events(EX.df, DV.df)

### Append CovT

events.df = append.CovT(events.df, CovT.df)

### Merge covariates

database.df = merge.Cov(events.df, Cov.l)

## Check for errors and report

## POST-MERGE HOOK GOES HERE
post.merge.hook()

## Filter, Transform, Exclude

database.df = post.merge.refactoring(database.df, fun.transform = post.transform, fun.filter = post.filter,
                                     fun.exclude = apply.exclusions)

## Write NONMEM file and DDS

write.csv(database.df,"database.csv")

