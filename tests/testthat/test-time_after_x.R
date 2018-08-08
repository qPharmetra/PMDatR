context("time-after")


describe("time after criteria works", {
  # real world type example

  it("after event flag", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   2,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/11/2017 12:00", "A",	100,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- c(0, 24, 240, 242)
    expect_equal(time_after_criteria(input,"TAEV", EVID == 2)$TAEV, output)
  })
  it("before first event flag should be zero", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   2,     0,  0.5,
      1,	"01/11/2017 12:00", "A",	100,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- c(0, 0, 0, 2)
    expect_equal(time_after_criteria(input,"TAEV", EVID == 2)$TAEV, output)
  })


})

describe("time after first dose works", {
  # real world type example

  it("works for time after first dose", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	0,  0,   1,     0,  0.3,
      1,	"01/01/2017 18:00",	"A",	0  ,  0,   2,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/04/2017 12:00",	"A",	0  , 24,   2,     0,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   4,     0,  0.5,
      1,	"01/11/2017 12:00", "A",	0  ,  0,   1,     0,  0.5,
      1,	"01/11/2017 14:00", "A",	0  ,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output=c(0, 8, 24, 74, 240, 242, 244)
    expect_equal(time_after_first_dose(input)$TAFD, output)
    expect_equal(time_after_first_dose(input, .criteria=AMT>0)$TAFD, output-24)
    expect_equal(time_after_first_dose(input, .name = "tafd")$tafd, output)
    input <- input %>% rename(ELTM=TIME)
    expect_equal(time_after_first_dose(input,.time="ELTM")$TAFD, output)
  })

#   it("works for time after first dose with excluded dose", {
#     input <- tibble::tribble(
#       ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE, ~EXCL,
#       1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,    1,
#       1,	"01/01/2017 18:00",	"A",	0  ,  0,   2,     0,  0.3,    0,
#       1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,    0,
#       1,	"01/04/2017 12:00",	"A",	0  , 24,   2,     0,  0.2,    0,
#       1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,    0,
#       1,	"01/11/2017 12:00", "A",	0  ,  0,   1,     0,  0.5,    0,
#       1,	"01/11/2017 14:00", "A",	0  ,  0,   1,     0,  0.5,    0
#     ) %>% dplyr::mutate(
#       TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
#     )
#     expect_equal(time_after_first_dose(input, .exclude = EXCL==1)$TAFD, c(0, 8, 24, 74, 240, 242, 244))
#   })

  it("works for time after dose", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/01/2017 18:00",	"A",	0  ,  0,   2,     0,  0.3,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/11/2017 12:00", "A",	0  ,  0,   1,     0,  0.5,
      1,	"01/11/2017 14:00", "A",	0  ,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(time_after_dose(input)$TAD, c(0, 8, 0, 0, 0))
  })

#   it("works for time after dose with excluded dose", {
#     input <- tibble::tribble(
#       ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE, EXCL,
#       1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,    1,
#       1,	"01/01/2017 18:00",	"A",	0  ,  0,   2,     0,  0.3,    0,
#       1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,    0,
#       1,	"01/11/2017 12:00", "A",	0  ,  0,   1,     0,  0.5,    0,
#       1,	"01/11/2017 14:00", "A",	0  ,  0,   1,     0,  0.5,    0
#     ) %>% dplyr::mutate(
#       TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
#     )
#     expect_equal(time_after_dose(input, .excl=EXCL==1)$TAD, c(0, 8, 0, 2, 4))
#   })

  it("works for time after dose with expanding addl", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/01/2017 18:00",	"A",	0  ,  0,   2,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/04/2017 12:00",	"A",	0  , 24,   2,     0,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/11/2017 12:00", "A",	0  ,  0,   4,     0,  0.5,
      1,	"01/11/2017 14:00", "A",	0  ,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(time_after_dose(input)$TAD, c(0, 8, 0, 2, 0, 0, 0))
    expect_equal(time_after_dose(input, .criteria = AMT>0)$TAD, c(0, 8, 0, 2, 0, 2, 4))
  })

  it("works for time of first dose", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/01/2017 18:00",	"A",	0  ,  0,   2,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/04/2017 12:00",	"A",	0  , 24,   2,     0,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/11/2017 12:00", "A",	0  ,  0,   1,     0,  0.5,
      1,	"01/11/2017 14:00", "A",	0  ,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(time_of_first_dose(input)$FDDTTM, rep(input$TIME[1],7))
  })

  it("works for time of first dose with addnl criteria", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/01/2017 18:00",	"A",	0  ,  0,   2,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/04/2017 12:00",	"A",	0  , 24,   2,     0,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/11/2017 12:00", "A",	0  ,  0,   1,     0,  0.5,
      1,	"01/11/2017 14:00", "A",	0  ,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(time_of_first_dose(input,"FDDTTM",AMT>0 & II>0)$FDDTTM, rep(input$TIME[3],7))
  })
})

describe("relative time works", {
  # real world type example

  it("works creating RTIME column", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/01/2017 18:00",	"A",	0  ,  0,   2,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/04/2017 12:00",	"A",	0  , 24,   2,     0,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/11/2017 12:00", "A",	0  ,  0,   1,     0,  0.5,
      1,	"01/11/2017 14:00", "A",	0  ,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(relative_time(input)$RTIME, c(0, 8, 24, 74, 240, 242, 244))
  })
  it("works to overwrite TIME column", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/01/2017 18:00",	"A",	0  ,  0,   2,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/04/2017 12:00",	"A",	0  , 24,   2,     0,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/11/2017 12:00", "A",	0  ,  0,   1,     0,  0.5,
      1,	"01/11/2017 14:00", "A",	0  ,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(relative_time(input, .name = "TIME")$TIME, c(0, 8, 24, 74, 240, 242, 244))
  })

})
