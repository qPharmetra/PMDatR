# test stat functions

library(testthat)
library(dplyr)
library(PMDatR)

context("domain stat functions")

x=c(NA,1:11,NA)
y=1:11

test_that("NA aware summary functions",{
  expect_equal(mean_(x),mean(y))
  expect_equal(median_(x),median(y))
  expect_equal(min_(x),min(y))
  expect_equal(max_(x),max(y))
  expect_equal(first_(x),first(y))
  expect_equal(last_(x),last(y))
  expect_equal(sum_(x),sum(y))

})
