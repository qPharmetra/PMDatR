
# PMDatR::bind_rows

context("Test bind_rows")

library(testthat)
library(dplyr)
library(PMDatR)


path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

#use Theoph for testing
th = Theoph %>% as_data_frame
th1=th %>% filter(Subject>3)
th2=th %>% filter(Subject<=3)

test_that("Can bind_rows on same types with no units", {
  df = bind_rows(th1,th2)
  expect_equal(df,th)
})

test_that("Can bind_rows on same types with units on one", {
  th3= th2 %>% mutate(conc=set_units(conc,"ng/mL"))
  df = bind_rows(th1,th3)
  expect_equal(df,th)
  expect_equal(units(df$conc),"ng/mL")
})

test_that("bind_rows fails wth incompatible units on column", {
  th3= th1 %>% mutate(conc=set_units(conc,"ng/mL"))
  th4= th2 %>% mutate(conc=set_units(conc,"mg"))
  expect_error(bind_rows(th3,th4))
})

test_that("Can bind_rows converts units", {
  th3= th1 %>% mutate(conc=set_units(conc,"ng/mL"))
  th4= th2 %>% mutate(conc=set_units(conc,"mg/mL"))
  df = bind_rows(th3,th4)
  expect_equal(as.double(df$conc),c(th3$conc,th4$conc*1e6))
  expect_equal(units(df$conc),"ng/mL")
})

test_that("Can bind_rows converts types: numeric+character", {
  th3= th1 %>% mutate(Subject = as.numeric(as.character(Subject)))
  th4= th2 %>% mutate(Subject = as.character(Subject))
  df = bind_rows(th3,th4)
  expect_equal(df,th %>% mutate(Subject = as.character(Subject)))
})

test_that("Can bind_rows converts types: logical+character", {
  th3= th1 %>% mutate(Logical = T)
  th4= th2 %>% mutate(Logical="TRUE")
  df = bind_rows(th3,th4)
  expect_equal(df,th %>% mutate(Logical = "TRUE"))
})

test_that("Can bind_rows converts types: mismatched columns", {
  th3= th1 %>% mutate(A = T)
  th4= th2 %>% mutate(B="TRUE")
  df = bind_rows(th3,th4)
  expect_equal(df$A, c(rep(T,nrow(th3)),rep(NA,nrow(th4))))
})
