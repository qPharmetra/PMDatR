# Unit tests for utilities

context("Diagnostics")

### Header section
library(testthat)
library(PMDatR)
library(dplyr)
library(tidyr)

path.testroot = "tests" # for testing in console
path.testroot = ".." # for build


## create fake domain object
df.DM = data_frame(USUBJID=1:6, TRTP=rep(c("TRT A", "TRT B"), each=3), TRTA=rep(c("TRT A", "TRT B"), each=3))
df.PC = data_frame(USUBJID=1:6, TRTP=rep(c("TRT A", "TRT B"), each=3), TRTA=rep(c("TRT A", "TRT B"), each=3)) %>%
  crossing(data_frame(TIME=1:5, DV=1:5))
df.EX = data_frame(USUBJID=1:6, TRTP=rep(c("TRT A", "TRT B"), each=3), TRTA=rep(c("TRT A", "TRT B"), each=3),
                   AMT=0)
df.LB = data_frame(USUBJID=1:6, TRTP=rep(c("TRT A", "TRT B"), each=3), TRTA=rep(c("TRT A", "TRT B"), each=3)) %>%
  crossing(data_frame(LBTESTCD=c("TEST1","TEST2","TEST3"), LBSTRESN=1:3))

DM = list(Domains=list(EX=list(name="EX", Data=df.EX, InputMappings=list(DefaultQuerySettings=list(ID="USUBJID"))),
                        PC=list(name="PC", Data=df.PC, InputMappings=list(DefaultQuerySettings=list(ID="USUBJID"))),
                        DM=list(name="DM", Data=df.DM, InputMappings=list(DefaultQuerySettings=list(ID="USUBJID"))),
                        LB=list(name="LB", Data=df.LB, InputMappings=list(DefaultQuerySettings=list(ID="USUBJID")))
           ))


test_that("find_mismatched_TRT: clean",{
  DM.test=DM
  expect_equivalent(find_mismatched_TRT(DM.test),list())

})

test_that("find_mismatched_TRT: mismatched TRT in domains",{
  DM.test=DM
  DM.test$Domains$DM$Data$TRTP[1]="bad"
  expect.df = data_frame(USUBJID=1L,TRT="TRTP",
                         TRTCODE="TRT A", DOMAIN=c("EX","PC","LB"))

  expect_equivalent(find_mismatched_TRT(DM.test),expect.df)

})

test_that("find_mismatched_TRT: extra TRT in PC",{
  DM.test=DM
  DM.test$Domains$PC$Data$TRTP[1:3]="bad"
  expect.df = data_frame(USUBJID=1L,TRT="TRTP",
                         TRTCODE="bad", DOMAIN="PC")

  expect_equivalent(find_mismatched_TRT(DM.test),expect.df)

})

test_that("find_mismatched_TRT: no TRT columns in master",{
  DM.test=DM
  DM.test$Domains$DM$Data = DM.test$Domains$DM$Data %>% select(USUBJID)
  # should have nothing to check
  expect_warning(out<-find_mismatched_TRT(DM.test),regexp = "No treatment columns found in master domain.+")
  expect_is(out,class="tbl")
  expect_true(nrow(out)==0)
})

test_that("find_mismatched_TRT: no TRT columns in domain PC, bad in EX",{
  DM.test=DM
  DM.test$Domains$PC$Data = DM.test$Domains$PC$Data %>% select(USUBJID)
  DM.test$Domains$EX$Data$TRTP[1:3]="bad"
  expect.df = data_frame(USUBJID=1:3,TRT="TRTP",
                         TRTCODE="bad", DOMAIN="EX")

  expect_equivalent(find_mismatched_TRT(DM.test),expect.df)
})

test_that("find_mismatched_TRT: returns unique rows ",{
  DM.test=DM
  DM.test$Domains$PC$Data$TRTP[DM.test$Domains$PC$Data$USUBJID==1]="bad"
  expect.df = data_frame(USUBJID=1L,TRT="TRTP",
                         TRTCODE="bad", DOMAIN="PC")

  expect_equivalent(find_mismatched_TRT(DM.test),expect.df)
})

### test date check



exFile = file.path(path.testroot, "testdata/data1/csv/ex.csv")
ex.df = read.csv(exFile)

test_that("check_iso_date_formats: all same format",{
  dates = ex.df$EXSTDTC
  PMDatR:::check_iso_date_formats(dates)
})

test_that("check_iso_date_formats: blank",{
  dates = ex.df$EXSTDTC
  dates[1]=""
  PMDatR:::check_iso_date_formats(dates)
})

test_that("check_iso_date_formats: all are blank",{
  dates = ex.df$EXSTDTC
  dates=rep("",10)
  PMDatR:::check_iso_date_formats(dates)
})

test_that("check_iso_date_formats: NA",{
  dates = ex.df$EXSTDTC
  dates[1]=NA
  PMDatR:::check_iso_date_formats(dates)
})

test_that("check_iso_date_formats: all are NA",{
  dates = ex.df$EXSTDTC
  dates=rep(NA,10)
  PMDatR:::check_iso_date_formats(dates)
})

#### check validnumeric

test_that("ValidNumeric: T for all numeric",{
  x=1:10
  attributes(x)$type="Numeric"
  expect_true(validNumeric(x))
})

test_that("ValidNumeric: NA for non-numeric type",{
  x=1:10
  attributes(x)$type="char"
  expect_equal(validNumeric(x),NA)
})

test_that("ValidNumeric: NA for no type",{
  x=1:10
  expect_equal(validNumeric(x),NA)
})

### Test diagnostic function
ex.dom = domain(yaml::yaml.load_file(file.path(path.testroot,"testdata","data1","ex.settings.yaml")))
ex.dom = load.domain(ex.dom,.fun=eval(parse(text=ex.dom$fnPreProc)))

test_that("domain diagnostics processes ideal data",{
  #create a domain object
  test.dom=ex.dom
  expect_silent(dgnost<-diagnostics.domain(test.dom))

})
