# test stat functions

library(testthat)
library(PMDatR)
library(dplyr)

context("default missing columns")

path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

df = mtcars %>% as_data_frame()

test_that("defaults non-existent column",{
  expect_equal(df %>% mutate(defcol = default_missing_column("defcol")) %>%
                 select(defcol),
               data_frame(defcol=rep(NA,nrow(mtcars))))
})

test_that("defaults non-existent column with user default",{
  expect_equal(df %>% mutate(defcol = default_missing_column("defcol", default_value =  123)) %>%
                 select(defcol),
               data_frame(defcol=rep(123,nrow(mtcars))))
})

test_that("preserves existing column",{
  expect_equal(df %>% mutate(defcol = default_missing_column("cyl")) %>%
                 select(defcol),
               mtcars %>% select(defcol=cyl))
})

test_that("can be called from within another function",{
  dummy_func = function(data){
    df %>% mutate(defcol = default_missing_column("cyl","")) %>%
      select(defcol)
  }
  expect_equal(dummy_func(mtcars),
               mtcars %>% select(defcol=cyl))
})

test_that("can be called from function stored in list",{
  dom=list()
  dom$dummy_func = function(data){
    df %>% mutate(defcol = default_missing_column("cyl","")) %>%
      select(defcol)
  }
  expect_equal(dom$dummy_func(mtcars),
               mtcars %>% select(defcol=cyl))
})

dm.settings = list(name="DM",
                   filepath=file.path(path.testroot,"testdata/data1/csv/dm.csv"),
                   InputMappings=list(Keys="ID",
                                      Filter="SUBJID==1",
                                      Columns=list(list(Name="ID", Mapping="USUBJID"),
                                                   list(Name="RFSTDTC", Mapping="iso_to_posix(RFSTDTC)"),
                                                   list(Name="SUBJID", Mapping="SUBJID"),
                                                   list(Name="AGE", Mapping="default_missing_column('AGE',25)"),
                                                   list(Name="newcol", Mapping="default_missing_column('newcol','newcol')")
                   )))

test_that("Test as part of domain load",{
  dm.dom = domain(dm.settings)
  dm.dom = load.domain(dm.dom)
  ## did we get expected row?
  expect_equal(nrow(dm.dom$Data),1)
  expect_equal(dm.dom$Data$AGE, 48)
  expect_equal(dm.dom$Data$SUBJID, 1)
  expect_equal(dm.dom$Data$newcol, "newcol")
  expect_equal(as.character(dm.dom$Data$RFSTDTC), "2013-12-18 09:00:00")
})

