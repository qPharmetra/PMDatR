context("Loading excel files")

library(testthat)
library(PMDatR)
library(dplyr)

path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

# utility function to use here
get.meta.col.names = function(meta) unlist(lapply(meta,"[[","name"))
# These tests do NOT check the dataframe for imported values.  They just check the load and that *stuff* came in.



test_that("load.domain() imports an xlsx file",{
  #create a domain object
  pc.dom = domain(list(name="PC",filepath=file.path(path.testroot,"/testdata/data1/xls/pc.xlsx")))
  pc.dom = load.domain(pc.dom)
  expect_is(pc.dom$Data,"tbl_df") #should have loaded with haven, so expect tbl_df
  expect_identical(colnames(pc.dom$Data),get.meta.col.names(pc.dom$inMeta)) #metadata captured the column names
  expect_true(pc.dom$Loaded) #Loaded should be true
  expect_equal(dim(pc.dom$Data),c(252,25)) #check the dimensions
})



