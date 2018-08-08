context("Loading text files")

library(testthat)
library(dplyr)
library(PMDatR)


path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

# utility function to use here
get.meta.col.names = function(meta) unlist(lapply(meta,"[[","name"))
# These tests do NOT check the dataframe for imported values.  They just check the load and that *stuff* came in.

pc.df = read.csv(file.path(path.testroot,"/testdata/data1/csv/pc.csv"))

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

testcsv = file.path(path.testroot,"./test.csv")

test_that("load.domain() imports a tab-delimited csv file",{
  # make a test file
  write.table(pc.df, testcsv , sep="\t", quote=F, row.names = F)
  #create a domain object
  test.dom = domain(list(name="PC",filepath=testcsv, FileSettings=list(
    sep="\t", header=T,quote=""  )))
  test.dom = load.domain(test.dom)
  expect_is(test.dom$Data,"tbl_df") #should have loaded with haven, so expect tbl_df
  expect_identical(colnames(test.dom$Data),get.meta.col.names(test.dom$inMeta)) #metadata captured the column names
  expect_true(test.dom$Loaded) #Loaded should be true
  expect_equal(dim(test.dom$Data),c(252,25)) #check the dimensions
  #remove the file
  file.remove(testcsv)
})


test_that("load.domain() imports a tab-delimited csv file skipping a few lines",{
  # make a test file
  write.table(pc.df, testcsv, sep="\t", quote=F, row.names = F)
  filecontents = readLines(testcsv)
  writeLines(c("Skip","These","4","Lines", filecontents), testcsv)
  #create a domain object
  test.dom = domain(list(name="PC",filepath= testcsv, FileSettings=list(
    sep="\t", header=T,skip=4  )))
  test.dom = load.domain(test.dom)
  expect_is(test.dom$Data,"tbl_df") #should have loaded with haven, so expect tbl_df
  expect_identical(colnames(test.dom$Data),get.meta.col.names(test.dom$inMeta)) #metadata captured the column names
  expect_true(test.dom$Loaded) #Loaded should be true
  expect_equal(dim(test.dom$Data),c(252,25)) #check the dimensions
  #remove the file
  file.remove(testcsv)
})

test_that("load.domain() imports a csv file ignoring extraneous options",{
  # make a test file
  write.csv(pc.df, testcsv, quote=T, row.names = F)
  #create a domain object
  test.dom = domain(list(name="PC",filepath= testcsv, FileSettings=list(  notused=T )))
  test.dom = load.domain(test.dom)
  expect_is(test.dom$Data,"tbl_df") #should have loaded with haven, so expect tbl_df
  expect_identical(colnames(test.dom$Data),get.meta.col.names(test.dom$inMeta)) #metadata captured the column names
  expect_true(test.dom$Loaded) #Loaded should be true
  expect_equal(dim(test.dom$Data),c(252,25)) #check the dimensions
  #remove the file
  file.remove(testcsv)
})

test_that("load.domain() imports a csv file and reshapes it",{
  # make a test file
  test.df = tibble::tribble(
    ~Subject, ~TRT, ~T_0, ~T_0.5, ~T_1.0,
    1,         "A",   0,   3.1,    9.2,
    2,         "B",  NA,  2.434,  8.34,
    3,         "A",  0.1, 3.213,  10.123)
  write.csv(test.df, testcsv, quote=F, row.names = F)
  #create a domain object
  test.dom = domain(list(name="PC",filepath= testcsv, FileSettings=list( id_cols="Subject, TRT") ))
  test.dom = load.domain(test.dom)
  expect_is(test.dom$Data,"tbl_df") #expect tbl_df
  expect_true(test.dom$Loaded) #Loaded should be true
  expect_equal(dim(test.dom$Data),c(9,4)) #check the dimensions
  #remove the file
  file.remove(testcsv)
})

test_that("load.domain() imports a csv file from settings yaml",{
  # make a test file
  dm.settings = yaml::yaml.load_file(file.path(path.testroot,"/testdata/data1/customcsv1.yaml"))
  #create a domain object
  dm.settings$InputMappings$PreMergeFile=""
  test.dom = domain(dm.settings)
  test.dom = load.domain(test.dom)
  expect_is(test.dom$Data,"tbl_df") #expect tbl_df
  expect_true(test.dom$Loaded) #Loaded should be true
  expect_equal(dim(test.dom$Data),c(10,28)) #check the dimensions
  #remove the file
  file.remove(testcsv)
})

