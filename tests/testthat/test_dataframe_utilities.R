library(testthat)
library(dplyr)
library(PMDatR)


context("Test Dataframe Utilities")

df = data_frame(x=0:10/10, y=1:11, z=1)

test_that("combine_rows_by_tolerance: does not combine with 0 tolerance",{
  input = df
  # no problems expected
  expect_equal(combine_rows_by_tol(input,"x",tol=0),df)

})

test_that("combine_rows_by_tolerance: combines",{
  input = df
  output= data_frame(x=c(0,.6), y=c(1L,7L), z=c(6L,5L))
  # no problems expected
  expect_equal(combine_rows_by_tol(input,"x",z=n(), tol=.5),output)

})

test_that("combine_rows_by_tolerance: combines, ignoring NA",{
  input = df
  input$x[2]=NA
  output= data_frame(x=c(0,NA,.6), y=c(1L,2L, 7L), z=c(5L,1L,5L))
  # no problems expected
  expect_equal(combine_rows_by_tol(input,"x",z=n(), tol=.5),output)

})

test_that("combine_rows_by_tolerance: respects grouping",{
  input = bind_rows(df %>% mutate(cmt=1), df %>% mutate(cmt=2)) %>% arrange(x,cmt) %>% group_by(cmt)
  output= data_frame(x=c(0,.6), y=c(1L,7L), z=c(6L,5L))
  output = bind_rows(output %>% mutate(cmt=1), output %>% mutate(cmt=2)) %>% arrange(x,cmt) %>% group_by(cmt)
  # no problems expected
  expect_equal(combine_rows_by_tol(input,"x",z=n(), tol=.5),output)

})



test_that("separate_rows: works with comma sep values",
          {
            input = data_frame(A=1:4, B=c("a","b, c", "d, e, f", "g"))
            output = separate_rows(input, "B")
            expect_equal(output$A,c(1,2,2,3,3,3,4))
            expect_equal(output$B,c(letters[1:7]))
          })

test_that("separate_rows: respects grouping",
          {
            input = data_frame(A=1:4, B=c("a","b, c", "d, e, f", "g"), C=c(1,1,2,2)) %>%
              group_by(C)
            output = separate_rows(input, "B")
            expect_equal(output$A,c(1,2,2,3,3,3,4))
            expect_equal(output$B,c(letters[1:7]))
            expect_equal(groups(output)[[1]], as.name("C"))
          })


##### time_varying_codes:
input <- tibble::tribble(
  ~ID,	~CMTRT,	~CMSTDTC,	~CMENDTC,	~CMENRF,
  "1000",	"AA",	"2016-10-18",	"2016-11-18", "" ,
  "1000",	"BB",	"2017-03-14", "",	 	"ONGOING",
  "1000",	"BB",	"2017-03-17", "2017-03-17",	 	"", # on/off after ongoing
  "1000",	"CC",	"2016-03-04", "",	 	"ONGOING",
  "1000",	"DD",	"2016-04-04",	"2016-04-04", "",
  "1000",	"DD",	"2016-04-04",	"2016-04-04", "",	 #duplicate
  "1000",	"GG",	"2016-04-15",	"2016-05-20", "",
  "1000",	"GG",	"2016-04-17",	"2016-04-18", "", # start and stop within another start-stop
  "1000",	"HH",	"2016-05-06",	"2016-05-08", "",
  "1000",	"HH",	"2016-05-09",	"", "ONGOING")	 %>%  #ongoing after previous end
  dplyr::mutate(
    CMSTDTC = iso_to_posix(CMSTDTC),
    CMENDTC = iso_to_posix(CMENDTC, .time = "24:00")
  ) %>% distinct

check = structure(
  list(
    ID = c("1000", "1000", "1000", "1000", "1000", "1000", "1000", "1000", "1000", "1000"),
    CMSTDTC = structure(c(1457049600, 1459728000, 1459728000, 1460678400, 1462492800,
                          1462752000, 1460678400, 1476748800, 1476748800, 1489449600),
                        class = c("POSIXct", "POSIXt" ), tzone = "UTC"),
    CMENDTC = structure(c(NA, 1459814400, 1459814400, 1463788800, 1462752000, NA,
                          1463788800, 1479513600, 1479513600, NA),
                        class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    CMENRF = c("ONGOING", "", "", "", "", "ONGOING", "", "", "", "ONGOING"),
    TIME = structure(c(1457049600, 1459728000, 1459814400, 1460678400, 1462492800,
                       1462752000, 1463788800, 1476748800, 1479513600, 1489449600),
                     class = c("POSIXct", "POSIXt" ), tzone = "UTC"),
    AA = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
    BB = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 2),
    CC = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
    DD = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    GG = c(0, 0, 0, 1, 1, 1, 0, 0, 0, 0),
    HH = c(0, 0, 0, 0, 1, 2, 2, 2, 2, 2)),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA, -10L),
  .Names = c("ID", "CMSTDTC", "CMENDTC", "CMENRF", "TIME", "AA", "BB", "CC", "DD", "GG", "HH"))

test_that("time_varying_codes: works",{

  output=time_varying_codes(input,CMTRT,CMSTDTC,CMENDTC,start_val=ifelse(CMENRF=="ONGOING",2,1),
                            end_val=0, .sticky=2)
  expect_equal(output,check)
})

test_that("time_varying_codes: works with grouping",{
  input.1 = bind_rows(input %>% mutate(GROUP=1), input %>% mutate(GROUP=2)) %>% group_by(GROUP)
  check.1 = bind_rows(check %>% mutate(GROUP=1), check %>% mutate(GROUP=2)) %>% group_by(GROUP)
  output=time_varying_codes(input.1,CMTRT,CMSTDTC,CMENDTC,start_val=ifelse(CMENRF=="ONGOING",2,1),
                            end_val=0, .sticky=2)
  expect_equal(output,check.1)
  expect_equal(groups(output)[[1]],as.name("GROUP"))
})


test_that("time_varying_codes: works with messy data 1",{
  input <- tibble::tribble(
    ~ID,	~CMTRT,	~CMSTDTC,	~CMENDTC,	~CMENRF,
    "1000",	"MIND",	"2016-10-05",	"2016-11-17", "" ,
    "1000",	"MINH",	"2016-11-12", "2016-11-23",	 	"",
    "1000",	"MINH",	"2016-11-16", "2016-11-23",	 	"",
    "1000",	"MIND",	"2016-11-17", "2016-11-17",	 	"",
    "1000",	"MIND",	"2016-11-17",	"2016-12-17", "")	 %>%  #ongoing after previous end
    dplyr::mutate(
      CMSTDTC = iso_to_posix(CMSTDTC),
      CMENDTC = iso_to_posix(CMENDTC)
    ) %>% distinct


  check = structure(
    list(
      ID = c("1000", "1000", "1000", "1000"),
      CMSTDTC = structure(c(1475625600, 1478908800, 1479254400, 1479340800),
                          class = c("POSIXct", "POSIXt" ), tzone = "UTC"),
      CMENDTC = structure(c(1479340800, 1479859200, 1479859200, 1481932800),
                          class = c("POSIXct", "POSIXt"), tzone = "UTC"),
      CMENRF = c("", "", "", ""),
      TIME = structure(c(1475625600, 1478908800, 1479859200, 1481932800),
                       class = c("POSIXct", "POSIXt"), tzone = "UTC"),
      MIND = c(1, 1, 1, 0),
      MINH = c(0, 1, 0, 0)),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -4L),
    .Names = c("ID", "CMSTDTC", "CMENDTC", "CMENRF", "TIME", "MIND", "MINH")
    )

  output=time_varying_codes(input,CMTRT,CMSTDTC,CMENDTC,start_val=ifelse(CMENRF=="ONGOING",2,1),
                            end_val=0, .sticky=2)
  expect_equal(output,check)
})
