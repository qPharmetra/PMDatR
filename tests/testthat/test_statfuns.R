# test stat functions

library(testthat)
library(PMDatR)

context("domain stat functions")

x=c(NA,1:11,NA)
y=1:11
vals = c(letters[c(1,1,1,2,2,1,1,1,2,3,3,2,3,3,3,2,1)],rep(NA,100))
NAs = rep(NA,10)

test_that("NA aware summary functions",{
  expect_equal(mean_(x),mean(y))
  expect_equal(median_(x),median(y))
  expect_equal(min_(x),min(y))
  expect_equal(max_(x),max(y))
  expect_equal(first_(x),first(y))
  expect_equal(last_(x),last(y))
  expect_equal(sum_(x),sum(y))

  expect_equal(mode_(vals),"a")
  expect_equal(mode_(as.factor(vals)),"a")
  expect_equal(mode_(as.integer(as.factor(vals))),1)
  expect_equal(mode_(as.double(as.factor(vals))/7),1/7)

  expect_equal(mean_(NAs), NA)
  expect_equal(median_(NAs), NA)
  expect_equal(min_(NAs), NA)
  expect_equal(max_(NAs), NA)
  expect_equal(first_(NAs), NA)
  expect_equal(last_(NAs), NA)
  expect_equal(sum_(NAs), 0)
  expect_equal(mode_(NAs),NA)

})

test_that("mode_ breaks ties",{
  # all provided
  expect_equal(mode_(c("A","B","C","A","B","C", NA, NA), na.rm=F, tie.order=c("C","A","B", NA)), "C")
  # no ties in modes
  expect_equal(mode_(c("A","B","C","A","B","C", NA, NA), na.rm=F, tie.order=c("X","Y","Z")), "A")
  # break to NA
  expect_equal(mode_(c("A","B","C","A","B","C", NA, NA), na.rm=F, tie.order=c("X","Y","Z", NA)), NA_character_)
  # Mode is NA
  expect_equal(mode_(c("A","B","C","A","B", NA, NA, NA), na.rm=F, tie.order=c("C","A","B", NA)), NA_character_)
  # tie.order is empty
  expect_equal(mode_(c("A","B","C","A","B","C", NA, NA), na.rm=F, tie.order=c()), "A")
  # remove NA
  expect_equal(mode_(c("A","B","C","A","B", NA, NA, NA), na.rm=T, tie.order=c("C","A","B", NA)), "A")
})
