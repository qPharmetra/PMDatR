context("cfab")


describe("change from baseline works for a single ID", {
  # real world type example

  it("works for absolute change", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE, ~IBSL, ~CRE2,
      1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3, 1, NA,
      1,	"01/01/2017 12:00",	"A",	100,  0,   1,     0,  0.4, 1, .4,
      1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2, 0, .2,
      1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5, 0, .5
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output_first <- c(0, 0.1, -0.1, 0.2)
    output_first_bls <- c(0, 0.0, -0.1, 0.2)
    output_last <- c(-0.2, -0.1, -0.3, 0.0)
    output_last_bls <- c(0, 0, -0.2, 0.1)
    output_mean <- c(0, 0, -.15, .15)
    output_mean_na <- c(NA, 0, -.2, .1)
    # as of 2017/08/18 a request to have all values that match baseline set to 0,
    # regardless of the aggregation functionality, so therefore,
    # this example with no filter should be still set to 0
    expect_equal(cfb_abs(input, CRE, .agg_fn = dplyr::first)$CFB_CRE, output_first)
    expect_equal(cfb_abs(input, CRE, IBSL == 1, .agg_fn = dplyr::first)$CFB_CRE, output_first_bls)
    expect_equal(cfb_abs(input, CRE, .agg_fn = dplyr::last)$CFB_CRE, output_last)
    expect_equal(cfb_abs(input, CRE, IBSL == 1, .agg_fn = dplyr::last)$CFB_CRE, output_last_bls)
    expect_equal(cfb_abs(input, CRE, IBSL == 1, .agg_fn = mean)$CFB_CRE, output_mean)
    expect_equal(cfb_abs(input, CRE2, IBSL == 1, .agg_fn = mean)$CFB_CRE2, output_mean_na)
  })

  it("works for absolute change, with groups", {
      input <- tibble::tribble(
        ~ID,	~TIME,	         ~TRT, ~AMT, ~II, ~EVID, ~ADDL, ~CRE, ~IBSL,
        1,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.3, 1,
        1,	"01/01/2017 12:00",	"A",	100,  0,   1,     0,  0.4, 1,
        1,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.2, 0,
        1,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.5, 0,
        2,	"01/01/2017 10:00",	"A",	100,  0,   1,     0,  0.2, 1,
        2,	"01/01/2017 12:00",	"A",	100,  0,   1,     0,  0.4, 1,
        2,	"01/02/2017 10:00",	"A",	100, 24,   1,     8,  0.1, 0,
        2,	"01/11/2017 10:00", "A",	100,  0,   1,     0,  0.6, 0
      ) %>% dplyr::mutate(
        TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
      )
      output_mean <- c(0, 0, -0.15, 0.15, -0, 0, -0.2, 0.3)
      output_first <- c(0, 0.1, -0.1, 0.2, 0, .2, -.1, .4)
      output_first_bls <- c(0, 0, -0.1, 0.2, 0, 0, -.1, .4)
      expect_equal(cfb_abs(input, CRE, .agg_fn = dplyr::first)$CFB_CRE, output_first)
      expect_equal(cfb_abs(input, CRE, IBSL == 1, .agg_fn = dplyr::first)$CFB_CRE, output_first_bls)
      expect_equal(cfb_abs(input, CRE, IBSL == 1, .agg_fn = mean)$CFB_CRE, output_mean)
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
