# Unit tests for time transformations

context("Time Transformations")

### Header section
library(testthat)
library(PMDatR)

test_that("elapsed time processes POSIXct",{
  #create POSIXct vector with known differences
  x = Sys.time() + c(0:10)*3600
  expect_equivalent(as.numeric(elapsed.time(x)),0:10) # values should match 0:10 hours
  expect_equivalent(as.numeric(elapsed.time(x, units="s")),0:10*3600) # values should match 0:10 sec
})

test_that("elapsed time errors for non POSIXct",{
  #create a domain object
  x =  c(0:10)*3600
  expect_error(as.numeric(elapsed.time(x))) # should error
})

### LOCF ###

test_that("locf carries forward",{
  #create a vector with NA values
  x = c(1,NA,2,3,NA,NA)
  expect_equal(locf(x),c(1,1,2,3,3,3))
})

test_that("locf carries backward when first value missing",{
  #create a vector with NA values
  x = c(NA,NA,2,3,NA,NA)
  expect_equal(locf(x),c(2,2,2,3,3,3))
})

test_that("locf carries forward when last value not missing",{
  #create a vector with NA values
  x = c(1,NA,2,3,NA,4)
  expect_equal(locf(x),c(1,1,2,3,3,4))
})

test_that("locf uses alternate missing value",{
  #create a vector with empty values
  x = c(1,"",2,3,"","")
  expect_equal(locf(x, na=""),as.character(c(1,1,2,3,3,3)))
})

test_that("locf works with multiple na flags",{
  #create a vector with empty values
  x = c(1,"",2,3,"",".")
  expect_equal(locf(x, na=c("",".")),as.character(c(1,1,2,3,3,3)))
  x = c(1,NA,2,3,"",".")
  expect_equal(locf(x, na=c("",".",NA)),as.character(c(1,1,2,3,3,3)))
})

### iso_to_posix
test_that("iso_to_posix process data + time",{
  x=iso_to_posix("2016-10-03T21:22:45")
  expect_equal(anytime::iso8601(x),"2016-10-03 21:22:45")
})

test_that("iso_to_posix processes date with filled in time", {
  x=iso_to_posix(c("2016-10-03","2016-10-03T12:34"),"09:00")
  expect_equal(anytime::iso8601(x), c("2016-10-03 09:00:00","2016-10-03 12:34:00"),"09:00")
})

test_that("iso_to_posix processes date with filled in time", {
  expect_warning(x<-iso_to_posix(c("2016-10-03","2016-10-03","2016-10-03"), c("09:00", "10:10")))
  #gives a warning about length of .times
  expect_equal(anytime::iso8601(x), c("2016-10-03 09:00:00","2016-10-03 10:10:00","2016-10-03 09:00:00"))
})

test_that("iso_to_posix processes NA", {
  expect_warning(x<-iso_to_posix(c(NA,"2016-10-03","2016-10-03"), c("09:00", "10:10")))
  #gives a warning about length of .times
  expect_equal(anytime::iso8601(x), c(NA,"2016-10-03 10:10:00","2016-10-03 09:00:00"))
})

test_that("iso_to_posix processes empty", {
  expect_warning(x<-iso_to_posix(c("","2016-10-03","2016-10-03"), c("09:00", "10:10")))
  #gives a warning about length of .times
  expect_equal(anytime::iso8601(x), c(NA,"2016-10-03 10:10:00","2016-10-03 09:00:00"))
})

test_that("iso_to_posix processes NA time string", {
  expect_warning(x<-iso_to_posix(c("2016-10-03","2016-10-03","2016-10-03"), c(NA, "10:10")))
  #gives a warning about length of .times
  expect_equal(anytime::iso8601(x), c("2016-10-03 00:00:00","2016-10-03 10:10:00","2016-10-03 00:00:00"))
})

test_that("iso_to_posix processes empty time string", {
  expect_warning(x<-iso_to_posix(c("2016-10-03","2016-10-03","2016-10-03"), c("", "10:10")))
  #gives a warning about length of .times
  expect_equal(anytime::iso8601(x), c("2016-10-03 00:00:00","2016-10-03 10:10:00","2016-10-03 00:00:00"))
})

# Test ISO durations

durs = .(PT1H,-PT1H,PT.5H,-PT.5H,P1D,PT24H,P1W,PT336H)



### conditional_values

test_that("conditional_values works with vector",{
  #create a vector with empty values
  x = c(1,1,2,2)
  expect_equal(conditional_values(a=x==1,b=x==2),c("a","a","b","b"))
  x = c(1,NA,2,2)
  expect_equal(conditional_values(a=x==1,b=x==2),c("a","","b","b"))
  expect_equal(conditional_values(a=x==1,b=x==2,c=is.na(x)),c("a","c","b","b"))
})

test_that("conditional_values multiple successes takes first or last",{
  #create a vector with empty values
  DV = c(1,2,3,NA,"BQL","No Sample",4)
  # Unoh, all non-numeric are BADDV.
  expect_equal(conditional_values(BADDV=!is.number(DV), BQL=DV=="BQL",MISSING=is.na(DV),default="OK"),
               c("OK","OK","OK","BADDV","BADDV","BADDV","OK"))
  # let's reverse order of search
  expect_equal(conditional_values(BADDV=!is.number(DV), BQL=DV=="BQL",MISSING=is.na(DV),default="OK",
                                 method="last"),
               c("OK","OK","OK","MISSING","BQL","BADDV","OK"))
})

test_that("conditional_values works with data frame",{
  #create a vector with empty values
  df = data.frame(CLCR=c(30, 65, 135, 120), GEN1 = c(1,2,1,2))
  expect_equal(conditional_values(high=df$CLCR<60 & df$GEN1==1, default="low"),c("high","low","low","low"))
})

test_that("conditional_values works with data frame in dplyr",{
  library(dplyr)
  #create a vector with empty values
  df = data.frame(CLCR=c(30, 65, 135, 120), GEN1 = c(1,2,1,2))
  df = df %>% mutate(risk=conditional_values(
    high=CLCR<60 & GEN1==1,
    default="low")
  )
  expect_equal(df$risk, c("high","low","low","low"))
})

