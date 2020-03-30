
# units prototype code and test

context("Units - component and integration tests")

library(testthat)
library(PMDatR)
library(dplyr)

path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

filepath = file.path(path.testroot,"/testdata/data1/csv/pc.csv")
df = read.csv(filepath)

test_that("units can be set on a column", {
  units.df = getDomain(df,ID=USUBJID, TIME=iso_to_posix(PCDTC), DV=set_units(PCSTRESN,PCSTRESU[1]))
  expect_equal(units(units.df$DV),"ng/mL")
})

test_that("getDV handles units in column",{
  units.df = getDV(df,ID=USUBJID, TIME=iso_to_posix(PCDTC), DV=PCSTRESN, Units=PCSTRESU)
  expect_equal(units.df$DVU,df$PCSTRESU)
})

test_that("getDV handles units as literal",{
  units.df = getDV(df,ID=USUBJID, TIME=iso_to_posix(PCDTC), DV=PCSTRESN, Units=ng/mL)
  expect_equal(units.df$DVU[1],df$PCSTRESU[1])
})

filepath = file.path(path.testroot,"/testdata/data1/csv/ex.csv")
df = read.csv(filepath)

test_that("getIndividualDosing handles units in column",{
  units.df = getIndividualDoses(df,ID=USUBJID, TIME=iso_to_posix(EXSTDTC), AMT=EXDOSE, Units=EXDOSU)
  expect_equal(units.df$AMTU,df$EXDOSU)
})

test_that("getIndividualDosing handles units as literal",{
  units.df = getIndividualDoses(df,ID=USUBJID, TIME=iso_to_posix(EXSTDTC), AMT=EXDOSE, Units=mg)
  expect_equal(units.df$AMTU,df$EXDOSU)
})

filepath = file.path(path.testroot,"/testdata/data1/csv/lb.csv")
df = read.csv(filepath)

test_that("getCov handles stacked data without units",{
  units.df = getCov(df, cov.filter = LBBLFL=="Y" & USUBJID==first(USUBJID), ID=USUBJID, cov.col=LBTESTCD,
                    cov.val=LBSTRESN, ALB=ALB, BILI=BILI, EPOCH=EPOCH, cov.keys=c("ID", "EPOCH"))
  expect_equal(unit_cols(units.df),NULL)
})

test_that("getCov handles stacked data with units",{
  units.df = getCov(df, cov.filter = LBBLFL=="Y" & USUBJID==first(USUBJID), ID=USUBJID, cov.col=LBTESTCD,
                    cov.val=LBSTRESN, ALB=ALB, BILI=BILI, EPOCH=EPOCH, cov.keys=c("ID", "EPOCH"), Units=LBSTRESU)
  expect_equal(units(units.df$ALB),"g/L")
  expect_equal(units(units.df$BILI),"umol/L")
})

test_that("getCovT handles stacked data without units",{
  units.df = getCovT(df, ID=USUBJID, TIME=iso_to_posix(LBDTC),
                    covT.col=LBTESTCD, covT.val=LBSTRESN, ALB=ALB, BILI=BILI)
  expect_equal(unit_cols(units.df),NULL)
})

test_that("getCov handles stacked data with units",{
  units.df = getCovT(df,  ID=USUBJID, TIME=iso_to_posix(LBDTC),
                     covT.col=LBTESTCD, covT.val=LBSTRESN, ALB=ALB, BILI=BILI, Units=LBSTRESU)
  expect_equal(units(units.df$ALB),"g/L")
  expect_equal(units(units.df$BILI),"umol/L")
})

test_that("unit columns can be used in post_transform",{
  XXX = function(alb,bili){
    x=alb/bili
    convert(x,"g/umol")
  }
  units.df = getCovT(df,  ID=USUBJID, TIME=iso_to_posix(LBDTC),
                     covT.col=LBTESTCD, covT.val=LBSTRESN, ALB=ALB, BILI=BILI, Units=LBSTRESU)
  expect_equal(units(units.df$ALB),"g/L")
  expect_equal(units(units.df$BILI),"umol/L")
  expect_equal(units((mutate(units.df,x=XXX(ALB,BILI)) %>% fill_locf(x))$x), "g/umol")
})
