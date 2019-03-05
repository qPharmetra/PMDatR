library(testthat)
library(PMDatR)
library(dplyr)

context("transforms_fill")

path.testroot = "tests" # for testing in console
path.testroot = ".." # for build


### quick test
set.seed(12345)
test1 = data_frame(ID=1,TIME=0:10,BW=runif(11,50,80))

test_that("Fill value replaces with a constant value",
          {

          })

test_that("Fill value replaces with an expression",
          {
            df=test1
            df$BW[4:6]=NA
            df=  df %>% fill_NA(BW,mean(BW, na.rm=T))
            expect_equal(df$BW[4:6],rep(mean(df$BW,na.rm=T),3))

          })

test_that("Fill value replaces with an expression",
          {
            df=test1
            df$BW[4:6]=NA
            df=  df %>% fill_NA(BW,median(BW, na.rm=T))
            expect_equal(df$BW[4:6],rep(median(df$BW,na.rm=T),3))
          })

test_that("Fill value takes group with . function",
          {
            df=test1
            df$BW[4:6]=NA
            df=df %>% fill_NA(BW,99,groups=.(ID))
            expect_equal(df$BW[4:6],rep(99,3))
          })

test_that("Fill value takes 2 groups with . function",
          {
            df=test1
            df$CMT=ifelse(df$TIME<5,1,2)
            df$BW[4:6]=NA
            df=df %>% fill_NA(BW,median(BW,na.rm=T),groups=.(ID,CMT))
            expect_equal(df$BW[4:6],c(rep(median(df$BW[1:3]),2),median(df$BW[7:11])))
          })

test_that("fill_locf will fill forward",
          {
            df=test1
            df$BW[4:6]=NA
            df=df %>% fill_locf(BW)
            expect_equal(df$BW[4:6],rep(df$BW[3],3))
          })

test_that("fill_locf takes 2 groups with . function",
          {
            df=test1
            df$CMT=ifelse(df$TIME<5,1,2)
            df$BW[4:6]=NA
            df=df %>% fill_locf(BW,groups=.(ID,CMT))
            expect_equal(df$BW[4:6],df$BW[c(3,3,7)])
          })

test_that("fill_locf takes 2 groups with . function and tol=1",
          {
            df=test1
            df$CMT=ifelse(df$TIME<5,1,2)
            df$BW[4:6]=NA
            df$TIME[4]=df$TIME[4]-.5 #this will cause 5 to come back NA
            df=df %>% fill_locf(BW,groups=.(ID,CMT),tol=1)
            expect_equal(df$BW[4:6],df$BW[c(3,NA,7)])
          })

test_that("fill_nocb takes 2 groups with . function and tol=1",
          {
            df=test1
            df$CMT=ifelse(df$TIME<5,1,2)
            df$BW[4:6]=NA
            df$TIME[4]=df$TIME[4]-.5 #this will cause 4 and 5 to come back NA
            df=df %>% fill_nocb(BW,groups=.(ID,CMT),tol=1)
            expect_equal(df$BW[4:6],df$BW[c(NA,NA,7)])
          })

test_that("fill_occasion works",
          {
            test.df = data_frame(vals = c(1,3,2,2,3,2,1,1,1,3,3,2,1,2,1,1,2,2,2,1,2,1,2,1,2),
                                 evid = c(1,0,0,0,0,0,1,0,0,1,0,1,1,0,1,0,0,0,0,1,0,1,0,1,0))
            test.df = test.df %>% fill_occasion(OCC, evid==1, evid==0 & vals==2)
            expect_equal(test.df$OCC,
                         c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3,
                           4, 4, 5, 5, 6, 6) )
          })
