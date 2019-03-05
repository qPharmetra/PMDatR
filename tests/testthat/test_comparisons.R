
library(testthat)
library(dplyr)
library(PMDatR)


context("Test Comparisons")

test_that("contains_one_of works",{
  expect_equal(contains_one_of(c("George Washington", "Washington DC", "King George"),  Washington, DC),
               c(TRUE,TRUE,FALSE))
  expect_equal(contains_one_of(c("George Washington", "Washington DC", "King George"),ing),
               c(TRUE,TRUE,TRUE))
  expect_equal(contains_one_of(c("George Washington", "Washington DC", "King George"), "^Wash.*$"),
               c(FALSE,TRUE,FALSE))
})
