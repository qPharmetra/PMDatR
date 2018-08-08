context("cfb")
library(testthat)
library(dplyr)
library(PMDatR)


describe("change from baseline works for a single ID", {
  # real world type example

  it("works for absolute change", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- c(0, -0.1, 0.2)
    expect_equal(cfb_abs(input, CRE)$CFB_CRE, output)
  })

 it("works for ratio change", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )

    expect_equal(cfb_ratio(input, CRE)$CFBR_CRE, c(0, -0.333, 0.667))
  })

 it("works for percent change", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )

    expect_equal(cfb_percent(input, CRE)$CFBP_CRE, c(0, -33.3, 66.7))
  })
})

describe("change from baseline works for a groups", {
  # real world type example

  it("works for absolute change", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/01/2017 10:00",	"B",	100,  0,   1,     0,  0.4,
      1,	"01/02/2017 10:00",	"B",	100, 24,   1,     8,  0.7,
      1,	"01/11/2017 10:00", "B",	100,  0,   1,     0,  0.1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output_id <- c(0, -0.1, 0.2, 0.1, 0.4, -0.2)
    output_id_trt <- c(0, -0.1, 0.2, 0, 0.3, -0.3)
    expect_equal(cfb_abs(input, CRE)$CFB_CRE, output_id)
    expect_equal(cfb_abs(input, CRE, groups = c("ID", "TRT"))$CFB_CRE, output_id_trt)
  })

 it("works for ratio change", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/01/2017 10:00",	"B",	100,  0,   1,     0,  0.4,
      1,	"01/02/2017 10:00",	"B",	100, 24,   1,     8,  0.7,
      1,	"01/11/2017 10:00", "B",	100,  0,   1,     0,  0.1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )

    expect_equal(cfb_ratio(input, CRE)$CFBR_CRE, c(0, -0.333, 0.667, 0.333, 1.333, -0.667))
    expect_equal(cfb_ratio(input, CRE, groups = c("ID", "TRT"))$CFBR_CRE, c(0, -0.333, 0.667, 0, 0.75, -0.75))
  })

 it("works for percent change", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/01/2017 10:00",	"B",	100,  0,   1,     0,  0.4,
      1,	"01/02/2017 10:00",	"B",	100, 24,   1,     8,  0.7,
      1,	"01/11/2017 10:00", "B",	100,  0,   1,     0,  0.1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(cfb_percent(input, CRE)$CFBP_CRE, c(0, -33.3, 66.7, 33.3, 133.3, -66.7))
    expect_equal(cfb_percent(input, CRE, groups = c("ID", "TRT"))$CFBP_CRE, c(0, -33.3, 66.7, 0, 75, -75))
  })
})

describe("get_baseline function", {
  # real world type example

  it("works with single value", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/01/2017 10:00",	"B",	100,  0,   1,     0,  0.4,
      1,	"01/02/2017 10:00",	"B",	100, 24,   1,     8,  0.7,
      1,	"01/11/2017 10:00", "B",	100,  0,   1,     0,  0.1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal((input %>% mutate(BL=get_baseline(CRE, TRUE, ID, fun.summ = first_)))$BL,
                  rep(.3,nrow(input)))
  })

  it("works with single value and grouping", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5,
      1,	"01/01/2017 10:00",	"B",	100,  0,   1,     0,  0.4,
      1,	"01/02/2017 10:00",	"B",	100, 24,   1,     8,  0.7,
      1,	"01/11/2017 10:00", "B",	100,  0,   1,     0,  0.1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal((input %>% mutate(BL=get_baseline(CRE, TRUE, TRT, fun.summ = first_)))$BL,
                 rep(c(.3,.4),each=3))
  })

  it("works with mean value and grouping", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2,
      1,	"01/11/2017 10:00", "A",	100,  0,   0,     0,  0.5,
      1,	"01/01/2017 10:00",	"B",	100,  0,   1,     0,  0.4,
      1,	"01/02/2017 10:00",	"B",	100, 24,   1,     8,  0.7,
      1,	"01/11/2017 10:00", "B",	100,  0,   0,     0,  0.1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal((input %>% mutate(BL=get_baseline(CRE, EVID==1, TRT)))$BL,
                 rep(c(.25,.55),each=3))
  })

  it("works with mean value, grouping, and complex filter", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	0,  0,   2,     0,  0.3,
      1,	"01/02/2017 10:30",	"A",	100, 0,   1,     8,  0.2,
      1,	"01/02/2017 11:00", "A",	0,  0,   0,     0,  0.5,
      1,	"01/03/2017 15:31",	"B",	0,  0,   2,     0,  0.4,
      1,	"01/04/2017 14:20",	"B",	100, 0,   1,     8,  0.7,
      1,	"01/04/2017 16:00", "B",	0,  0,   0,     0,  0.1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    ) %>% group_by(ID,TRT) %>% transform(NDOSE = pmax(1,cumsum(EVID==1))) %>%
      time_after_first_dose(groups=.(ID,TRT,NDOSE), .name='TAD')
    expect_equal((input %>% mutate(BL=get_baseline(CRE, EVID==2 & TAD<=0, TRT)))$BL,
                 rep(c(.3,.4),each=3))
  })

  it("works with mean value, grouping, complex filter, missing baseline", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE,
      1,	"01/01/2017 10:00",	"A",	0,  0,   2,     0,  0.3,
      1,	"01/02/2017 10:30",	"A",	100, 0,   1,     0,  0.2,
      1,	"01/02/2017 11:00", "A",	0,  0,   0,     0,  0.5,
      1,	"01/03/2017 15:31",	"B",	0,  0,   2,     0,  0.4,
      1,	"01/04/2017 14:20",	"B",	100, 0,   1,     0,  0.7,
      1,	"01/04/2017 16:00", "B",	0,  0,   0,     0,  0.1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    ) %>% group_by(ID,TRT) %>% transform(NDOSE = pmax(1,cumsum(EVID==1))) %>%
      time_after_first_dose(groups=.(ID,TRT,NDOSE), .name='TAD')
    expect_equal((input %>% mutate(BL=get_baseline(CRE, EVID==2 & TAD<=-23, TRT)))$BL,
                 rep(c(.3,NA),each=3))
  })

})
