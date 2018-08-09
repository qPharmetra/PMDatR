library(testthat)
library(PMDatR)
library(dplyr)

context("Queries")

path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

# utility function to use here
get.meta.col.names = function(meta) unlist(lapply(meta,"[[","name"))

# example.yaml contains valid settings for different query types in data1 example

settings = yaml::yaml.load_file(file.path(path.testroot, "testdata/data1/Units.yaml"))
#spoof PC metadata
metadata_pc=list()
metadata_pc$Source="spoof"
metadata_pc$Columns=lapply(.(USUBJID,PCSTAT,PCSTRESN),function(x) list(name=x))

test_that("Check structure DV",{
  #create queryDV
  qdv = Query_DV(settings$DependentVariables[[1]], metadata_pc)
  expect_equal(qdv$valid,T)
})

test_that("DV no Name",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$Name=NULL
  #create queryDV
  qdv = Query_DV(badSettings)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q1.+")
})

test_that("DV bad Name",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$Name="1bad"
  #create queryDV
  qdv = Query_DV(badSettings)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q2.+")
})

test_that("DV no domain",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$Domain=NULL
  #create queryDV
  qdv = Query_DV(badSettings)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q3.+")
})

test_that("DV bad domain",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$Domain="1bad"
  #create queryDV
  qdv = Query_DV(badSettings)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q4.+")
})

test_that("DV bad Filter",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$Filter="not an expression"
  #create queryDV
  qdv = Query_DV(badSettings)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q5.+")
})


test_that("DV bad Filter",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$Filter="xx==yy+USUBJID"
  #create queryDV
  qdv = Query_DV(badSettings, settings$DomainMetaData[[4]])
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q6.+")
})

test_that("DV missing required mapping",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$ID=NULL
  #create queryDV
  qdv = Query_DV(badSettings)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q7.+")
})

test_that("DV missing required mapping",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$ID=""
  #create queryDV
  qdv = Query_DV(badSettings)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q8.+")
})

test_that("DV required mapping not parseable",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$ID="as.character(thing"
  #create queryDV
  qdv = Query_DV(badSettings)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q9.+")
})

test_that("DV required mapping has bad variables",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$ID="fun(nope, uhnuh, USUBJID)"
  #create queryDV
  qdv = Query_DV(badSettings, settings$DomainMetaData[[4]])
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q10.+")
})

test_that("DV missing optional mapping",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$CMT=NULL #this is actually OK
  #create queryDV
  qdv = Query_DV(badSettings, metadata_pc)
  expect_equal(qdv$valid,T)
})

test_that("DV optional mapping not parseable",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$CMT="as.character(thing"
  #create queryDV
  qdv = Query_DV(badSettings, metadata_pc)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q11.+")
})

test_that("DV optional mapping has bad variables",{
  badSettings = settings$DependentVariables[[1]]
  badSettings$CMT="fun(nope, uhnuh, USUBJID)"
  #create queryDV
  qdv = Query_DV(badSettings, settings$DomainMetaData[[4]])
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q12.+")
})


test_that("Cov additional column mapping NULL name",{
  badSettings = settings$Covariates[[1]]
  badSettings$Columns[[1]]$Name = NULL
  #create queryDV
  qdv = Query_Cov(badSettings, metadata_pc)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q13.+")
})

test_that("Cov additional column mapping bad name",{
  badSettings = settings$Covariates[[1]]
  badSettings$Columns[[1]]$Name = "1bad"
  #create queryDV
  qdv = Query_Cov(badSettings, metadata_pc)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q14.+")
})

test_that("Cov additional column mapping is empty",{
  badSettings = settings$Covariates[[1]]
  badSettings$Columns[[1]]$Mapping = ""
  #create queryDV
  qdv = Query_Cov(badSettings, metadata_pc)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q15.+")
})

test_that("TVCov additional column mapping mapping not parseable",{
  badSettings = settings$TVCovariates[[1]]
  badSettings$Columns[[1]]$Mapping = "as.character(thing"
  #create queryDV
  meta_data = settings$DomainMetaData[[3]]
  names(meta_data$Columns) = purrr::map_chr(meta_data$Columns, "name")
  qdv = Query_CovT(badSettings, meta_data )
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q16.+")
})

test_that("TVCov additional column mapping has bad variables",{
  badSettings = settings$TVCovariates[[1]]
  badSettings$Columns[[1]]$Mapping = "fun(nope, uhnuh, USUBJID)"
  #create queryDV
  meta_data = settings$DomainMetaData[[3]]
  names(meta_data$Columns) = purrr::map_chr(meta_data$Columns, "name")
  qdv = Query_CovT(badSettings, meta_data )
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q17.+")
})

test_that("TVCov additional column mapping can create new column and use it without failing checks",{
  badSettings = settings$TVCovariates[[1]]
  badSettings$Columns[[3]] = list(Name="NewCol",Mapping="BILI+CREAT",Type="Unknown")
  badSettings$Columns[[4]] = list(Name="NewCol2",Mapping="NewCol",Type="Unknown")
  #create queryDV
  meta_data = settings$DomainMetaData[[3]]
  names(meta_data$Columns) = purrr::map_chr(meta_data$Columns, "name")
  qdv = Query_CovT(badSettings, meta_data )
  expect_equal(qdv$valid,T)

})


test_that("DV multiple errors",{
  badSettings = settings$Covariates[[1]]
  badSettings$Columns[[1]]$Name = "..."
  badSettings$Columns[[2]]$Mapping = NULL
  badSettings$Columns[[3]]$Mapping = "nope())"
  badSettings$Name="1bad"
  badSettings$ID=NULL
  #create queryDV
  qdv = Query_Cov(badSettings)
  expect_equal(qdv$valid,F)
  expect_match(qdv$errors[[1]],"Error Q2.+")
  expect_match(qdv$errors[[2]],"Error Q7.+")
  expect_match(qdv$errors[[3]],"Error Q14.+")
  expect_match(qdv$errors[[4]],"Error Q13.+")
  expect_match(qdv$errors[[5]],"Error Q16.+")
})


