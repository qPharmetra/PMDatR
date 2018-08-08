context("Loading domain files")

library(testthat)
library(PMDatR)
library(dplyr)

path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

# utility function to use here
get.meta.col.names = function(meta) unlist(lapply(meta,"[[","name"))
# These tests do NOT check the dataframe for imported values.  They just check the load and that *stuff* came in.

test_that("load.domain() imports a sas7bdat file",{
  #create a domain object
  pc.dom = domain(list(name="PC",filepath=file.path(path.testroot,"/testdata/data1/sas/pc.sas7bdat")))
  pc.dom = load.domain(pc.dom)
  expect_is(pc.dom$Data,"tbl_df") #should have loaded with haven, so expect tbl_df
  expect_identical(colnames(pc.dom$Data),get.meta.col.names(pc.dom$inMeta)) #metadata captured the column names
  expect_true(pc.dom$Loaded) #Loaded should be true
  expect_equal(dim(pc.dom$Data),c(252,25)) #check the dimensions
})

test_that("load.domain() imports an csv file",{
  #create a domain object
  pc.dom = domain(list(name="PC",filepath=file.path(path.testroot,"/testdata/data1/csv/pc.csv"), FileSettings=list(
    sep=",", header=T  )))
  pc.dom = load.domain(pc.dom)
  expect_is(pc.dom$Data,"tbl_df") #should have loaded with haven, so expect tbl_df
  expect_identical(colnames(pc.dom$Data),get.meta.col.names(pc.dom$inMeta)) #metadata captured the column names
  expect_true(pc.dom$Loaded) #Loaded should be true
  expect_equal(dim(pc.dom$Data),c(252,25)) #check the dimensions
})

test_that("load.domain() imports an xlsx file",{
  #create a domain object
  pc.dom = domain(list(name="PC",filepath=file.path(path.testroot,"/testdata/data1/xls/pc.xlsx")))
  pc.dom = load.domain(pc.dom)
  expect_is(pc.dom$Data,"tbl_df") #should have loaded with haven, so expect tbl_df
  expect_identical(colnames(pc.dom$Data),get.meta.col.names(pc.dom$inMeta)) #metadata captured the column names
  expect_true(pc.dom$Loaded) #Loaded should be true
  expect_equal(dim(pc.dom$Data),c(252,25)) #check the dimensions
})

test_that("load.domain() imports an xpt file",{
  #create a domain object
  pc.dom = domain(list(name="PC",filepath=file.path(path.testroot,"/testdata/data1/xpt/pc.xpt")))
  pc.dom = load.domain(pc.dom)
  expect_is(pc.dom$Data,"tbl_df") #should have be converted by dplyr, so expect tbl_df
  expect_identical(colnames(pc.dom$Data),get.meta.col.names(pc.dom$inMeta)) #metadata captured the column names
  expect_true(pc.dom$Loaded) #Loaded should be true
  expect_equal(dim(pc.dom$Data),c(252,25)) #check the dimensions
})

## Add tests for fabricated data frames

### Look specifically at metadata

meta.df = data_frame(A=1:100, B=100:1/100, C=letters[1:100], D=NA)
write.csv(meta.df,"meta.csv", quote=F, row.names = F)

meta.dom=domain(list(name="meta", filepath="meta.csv"))
meta.dom = load.domain(meta.dom)

test_that("load domain processes meta data", {
  expect_identical(unlist(lapply(meta.dom$inMeta, function(x) x$name)), c("A","B","C", "D"))
  expect_identical(unlist(lapply(meta.dom$inMeta, function(x) x$storage)), c("Numeric","Numeric","Text","Logical"))
  expect_identical(unlist(lapply(meta.dom$inMeta, function(x) x$minimum)), c("1","0.01",NA,NA))
  expect_identical(unlist(lapply(meta.dom$inMeta, function(x) x$maximum)), c("100","1",NA,NA))
  expect_identical(unlist(lapply(meta.dom$inMeta, function(x) x$unique)), c(NA,NA,letters[1:5],NA))
})
