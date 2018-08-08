context("compare_mappings")

library(testthat)
library(PMDatR)
library(dplyr)

# path.testroot = "tests" # for testing in console
# path.testroot = ".." # for build
#
# # utility function to use here
# get.meta.col.names = function(meta) unlist(lapply(meta,"[[","name"))
# # These tests do NOT check the dataframe for imported values.  They just check the load and that *stuff* came in.
map1 = list(A=1,B="b",Columns=list(list(Name="Alpha", Mapping="alpha(x)"),
                                   list(Name="Beta", Mapping="beta(x)")))
map2 = map1
test_that("compare_mappings finds no difference",{
  #create a domain object
  test.df = PMDatR:::compare_mappings(map1,map2)
  expect_equal(nrow(test.df),0) #should have loaded with haven, so expect tbl_df
})

test_that("compare_mappings finds differences",{
  #create a domain object
  test1 = map1
  test2 = map2
  test1$A = 2
  test2$Columns[[1]]$Mapping="exp(x)"
  output = tibble::tribble(~Setting, ~map1, ~map2,
                           "A", "2", "1",
                           "Columns.Alpha.Mapping", "alpha(x)", "exp(x)")
  test.df = PMDatR:::compare_mappings(test1,test2)
  expect_equal(test.df,output) #check the dimensions
})

test_that("compare_mappings finds removed in map2",{
  #create a domain object
  test1 = map1
  test2 = map2
  test2$Columns[[1]]=NULL
  output = tibble::tribble(~Setting, ~map1, ~map2,
                           "Columns.Alpha.Name", "Alpha", ".",
                           "Columns.Alpha.Mapping", "alpha(x)", ".")
  test.df = PMDatR:::compare_mappings(test1,test2)
  expect_equal(test.df,output) #check the dimensions
})

test_that("compare_mappings finds added in map2",{
  #create a domain object
  test1 = map1
  test2 = map2
  test2$Columns[[3]]=list(Name="Gamma", Mapping="gamma(x)")
  output = tibble::tribble(~Setting, ~map1, ~map2,
                           "Columns.Gamma.Name", ".", "Gamma",
                           "Columns.Gamma.Mapping", ".", "gamma(x)")
  test.df = PMDatR:::compare_mappings(test1,test2)
  expect_equal(test.df,output) #check the dimensions
})

