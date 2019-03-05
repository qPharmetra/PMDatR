context("addl")


describe("get_addl_dosing works as expected for QD", {
  # real world type example
  ex_dat1 <- tibble::tribble(
    ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,     ~II, ~EVID,
    1,	"12/14/2016 09:00",	"A",	100,	"ONCE",	FALSE,    0,    1,
    1,	"12/21/2016 10:12",	"A",	100,	"QD",	  FALSE,    24,   1,
    1,	"01/01/2017 10:00", "A",  0,	  "QD",	  TRUE,     24,   2,
    1,	"01/11/2017 07:33", "A",	100,	"QD",	  FALSE,    24,   1
  ) %>% dplyr::mutate(
    TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
  )
  # structure of ex_dat1 should be the following, with TIME set to UTC
  # # A tibble: 4 × 9
  #  ID                TIME   TRT   AMT  FREQ   MEX MEX__    II  EVID
  #<dbl>              <dttm> <chr> <dbl> <chr> <chr> <dbl> <dbl> <dbl>
  #  1 2016-12-14 09:00:00     A   100  ONCE     Y     0     0     1
  #  1 2016-12-21 10:12:00     A   100    QD     Y     0    24     1
  #  1 2017-01-01 00:00:00     A     0    QD     N     1    24     2
  #  1 2017-01-11 07:33:00     A   100    QD     Y     0    24     1

  it("works for simple QD dosing", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"01/01/2017 10:00",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"01/11/2017 10:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"01/01/2017 10:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"01/02/2017 10:00",	"A",	100,	"QD",	  FALSE,   24,   1,     8,
      1,	"01/11/2017 10:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for simple QD dosing 2", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"06/12/2017 08:30",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"08/21/2017 21:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"2017-06-12 08:30:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2017-06-12 21:00:00",	"A",	100,	"QD",	  FALSE,   24,   1,     69,
      1,	"2017-08-21 21:00:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for simple QD dosing 3", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"10/17/2016 21:00",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"11/16/2016 08:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"2016-10-17 21:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-19 08:00:00",	"A",	100,	"QD",	  FALSE,   24,   1,     27,
      1,	"2016-11-16 08:00:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for simple QD dosing 4, dose too close", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"10/17/2016 10:00",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"10/18/2016 08:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"2016-10-17 10:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-18 08:00:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for simple QD dosing 5, time slightly > II", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"10/17/2016 00:00",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"10/18/2016 10:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"2016-10-17 00:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-18 10:00:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })
  it("works for less simple QD dosing 6, actual doses within II of eachother", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"01/10/2017 11:10",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"02/05/2017 00:00", "A",	100,	"QD",	  FALSE,   24,   1,
      1,	"02/06/2017 00:00", "A",	100,	"QD",	  FALSE,   24,   1,
      1,	"02/07/2017 10:00", "A",	100,	"QD",	  FALSE,   24,   1,
      1,	"03/07/2017 00:00", "A",	100,	"QD",	  FALSE,   24,   1,
      1,	"03/08/2017 14:00", "A",	100,	"QD",	  FALSE,   24,   1,
      1,	"03/09/2017 09:30", "A",	100,	"QD",	  FALSE,   24,   1,
      1,	"04/29/2017 00:00", "A",	100,	"QD",	  FALSE,   24,   1,
      1,	"04/30/2017 00:00", "A",	100,	"QD",	  FALSE,   24,   1,
      1,	"05/01/2017 00:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"01/10/2017 11:10",	"A",	100,	"QD",	  FALSE,   0,   1,  0,
      1,	"01/11/2017 00:00",	"A",	100,	"QD",	  FALSE,   24,   1, 24,
      1,	"02/05/2017 00:00", "A",	100,	"QD",	  FALSE,   0,   1,  0,
      1,	"02/06/2017 00:00", "A",	100,	"QD",	  FALSE,   0,   1,  0,
      1,	"02/07/2017 10:00", "A",	100,	"QD",	  FALSE,   0,   1,  0,
      1,	"02/08/2017 00:00", "A",	100,	"QD",	  FALSE,   24,   1, 26,
      1,	"03/07/2017 00:00", "A",	100,	"QD",	  FALSE,   0,   1,  0,
      1,	"03/07/2017 14:00", "A",	100,	"QD",	  FALSE,   0,   1,  0,
      1,	"03/08/2017 14:00", "A",	100,	"QD",	  FALSE,   0,   1,  0,
      1,	"03/09/2017 09:30", "A",	100,	"QD",	  FALSE,   0,   1,  0,
      1,	"03/10/2017 00:00", "A",	100,	"QD",	  FALSE,   24,   1, 49,
      1,	"04/29/2017 00:00", "A",	100,	"QD",	  FALSE,   0,   1,  0,
      1,	"04/30/2017 00:00", "A",	100,	"QD",	  FALSE,   0,   1,  0,
      1,	"05/01/2017 00:00", "A",	100,	"QD",	  FALSE,   0,   1,  0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for simple QD dosing, dose at II/2", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"10/17/2016 00:00",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"10/18/2016 12:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"2016-10-17 00:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,  "2016-10-17 12:00:00",  "A",  100,  "QD",   FALSE,    0,   1,     0,
      1,	"2016-10-18 12:00:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for simple QD dosing, dose at II/2 tol now at .51", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"10/17/2016 00:00",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"10/18/2016 12:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"2016-10-17 00:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-18 12:00:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input, .tolII=.51), output)
  })

  it("zero tolerance with dose at II, tol=0", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"10/17/2016 00:00",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"10/18/2016 00:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"2016-10-17 00:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-17 00:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-18 00:00:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input, .tolII=.0), output)
  })

  it("zero tolerance with dose at II + delta, tol=0", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"10/17/2016 00:00",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"10/18/2016 00:01", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"2016-10-17 00:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-17 00:01:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-18 00:01:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input, .tolII=.0), output)
  })

  it("QD Dose at 2*II, tol=1", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"10/17/2016 00:00",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"10/19/2016 00:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"2016-10-17 00:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-18 00:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-19 00:00:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input, .tolII=1), output)
  })

  it("QD Dose at 2*II, tol=0", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"10/17/2016 00:00",	"A",	100,	"QD",	  FALSE,   24,   1,
      1,	"10/19/2016 00:00", "A",	100,	"QD",	  FALSE,   24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"2016-10-17 00:00:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"2016-10-17 00:00:00",	"A",	100,	"QD",	  FALSE,    24,   1,     1,
      1,	"2016-10-19 00:00:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input, .tolII=0), output)
  })

  it("works for QD with single missing dose", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"01/01/2017 10:00",	"A",	100,	"QD",	  FALSE,    24,   1,
      1,	"01/05/2017 10:00", "A",  0,	  "QD",	  TRUE,     24,   1,
      1,	"01/11/2017 10:00", "A",	100,	"QD",	  FALSE,    24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID, ~ADDL,
      1,	"01/01/2017 10:00",	"A",	100,	"QD",	  FALSE,    0,   1,     0,
      1,	"01/02/2017 10:00",	"A",	100,	"QD",	  FALSE,   24,   1,     2,
      1,	"01/05/2017 10:00", "A",  0,	  "QD",	  TRUE,     0,   1,     0,
      1,	"01/06/2017 10:00",	"A",	100,	"QD",	  FALSE,   24,   1,     4,
      1,	"01/11/2017 10:00", "A",	100,	"QD",	  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for QD with single missing dose at a different time", {

    # given a dosing time of a missed dose that does NOT correspond with the next dose time,
    # the time should not be adjusted to match. The 7:00 time should be
    # preserved, even though the next dose time is 10:00
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,     ~II, ~EVID,
      1,	"01/01/2017 10:00",	"A",	100,	"QD",	  FALSE,    24,   1,
      1,	"01/05/2017 07:00", "A",  0,	  "QD",	  TRUE,     24,   1,
      1,	"01/11/2017 10:00", "A",	100,	"QD",	  FALSE,    24,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,     ~II, ~EVID, ~ADDL,
      1,	"01/01/2017 10:00",	"A",	100,	"QD",	  FALSE,     0,   1,     0,
      1,	"01/02/2017 10:00",	"A",	100,	"QD",	  FALSE,    24,   1,     2,
      1,	"01/05/2017 07:00", "A",  0,	  "QD",	  TRUE,      0,   1,     0,
      1,	"01/06/2017 10:00",	"A",	100,	"QD",	  FALSE,    24,   1,     4,
      1,	"01/11/2017 10:00", "A",	100,	"QD",	  FALSE,     0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })
})


describe("get_addl_dosing works as expected for BID", {

  it("works for simple BID dosing", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"01/01/2017 10:00",	"A",	100,	"BID",  FALSE,   12,   1,
      1,	"01/11/2017 10:00", "A",	100,	"BID",  FALSE,   12,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,     ~II, ~EVID, ~ADDL,
      1,	"01/01/2017 10:00",	"A",	100,	"BID",	  FALSE,   0,   1,     0,
      1,	"01/01/2017 22:00",	"A",	100,	"BID",	  FALSE,  12,   1,     18,
      1,	"01/11/2017 10:00", "A",	100,	"BID",	  FALSE,   0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for simple BID dosing 2, too close", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"01/01/2017 00:00",	"A",	100,	"BID",  FALSE,   12,   1,
      1,	"01/01/2017 17:59", "A",	100,	"BID",  FALSE,   12,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,     ~II, ~EVID, ~ADDL,
      1,	"01/01/2017 00:00",	"A",	100,	"BID",	  FALSE,   0,   1,     0,
      1,	"01/01/2017 17:59",	"A",	100,	"BID",	  FALSE,  0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for simple BID dosing 2, just far enough", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,   ~II, ~EVID,
      1,	"01/01/2017 00:00",	"A",	100,	"BID",  FALSE,   12,   1,
      1,	"01/01/2017 18:00", "A",	100,	"BID",  FALSE,   12,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,     ~II, ~EVID, ~ADDL,
      1,	"01/01/2017 00:00",	"A",	100,	"BID",	  FALSE,   0,   1,     0,
      1,	"01/01/2017 06:00",	"A",	100,	"BID",	  FALSE,  0,   1,     0,
      1,	"01/01/2017 18:00",	"A",	100,	"BID",	  FALSE,  0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for BID with single missing dose", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,     ~II, ~EVID,
      1,	"01/01/2017 10:00",	"A",	100,	"BID",  FALSE,    12,   1,
      1,	"01/05/2017 10:00", "A",  0,	  "BID",  TRUE,     12,   1,
      1,	"01/11/2017 10:00", "A",	100,	"BID",  FALSE,    12,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,  ~II, ~EVID, ~ADDL,
      1,	"01/01/2017 10:00",	"A",	100,	"BID",  FALSE,  0,   1,     0,
      1,	"01/01/2017 22:00",	"A",	100,	"BID",  FALSE, 12,   1,     6,
      1,	"01/05/2017 10:00", "A",  0,	  "BID",  TRUE,   0,   1,     0,
      1,	"01/05/2017 22:00",	"A",	100,	"BID",  FALSE, 12,   1,     10,
      1,	"01/11/2017 10:00", "A",	100,	"BID",  FALSE,  0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

  it("works for BID with multiple non-consecutive missing doses", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX, ~MEX__, ~II, ~EVID,
      1,	"01/01/2017 10:00",	"A",	100,	"BID",  FALSE,    0,    12,   1,
      1,	"01/05/2017 10:00", "A",  0,	  "BID",  TRUE,    1,    12,   1,
      1,	"01/07/2017 22:00", "A",  0,	  "BID",  TRUE,    1,    12,   1,
      1,	"01/11/2017 10:00", "A",	100,	"BID",  FALSE,    0,    12,   1,
      1,	"01/11/2017 22:00", "A",	100,	"BID",  FALSE,    0,    12,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,    ~II, ~EVID, ~ADDL,
      1,	"01/01/2017 10:00",	"A",	100,	"BID",  FALSE,    0,   1,     0,
      1,	"01/01/2017 22:00",	"A",	100,	"BID",  FALSE,   12,   1,     6,
      1,	"01/05/2017 10:00", "A",  0,	  "BID",  TRUE,     0,   1,     0,
      1,	"01/05/2017 22:00",	"A",	100,	"BID",  FALSE,   12,   1,     3,
      1,	"01/07/2017 22:00", "A",  0,	  "BID",  TRUE,     0,   1,     0,
      1,	"01/08/2017 10:00", "A",  100,  "BID",  FALSE,   12,   1,     5,
      1,	"01/11/2017 10:00", "A",	100,	"BID",  FALSE,    0,   1,     0,
      1,	"01/11/2017 22:00", "A",	100,	"BID",  FALSE,    0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })

it("works for BID where next dose sequence time is different than previous", {
   input <- tibble::tribble(
     ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,    ~II, ~EVID,
     1,	"01/01/2017 10:00",	"A",	100,	"BID",  FALSE,    12,   1,
     1,	"01/07/2017 22:00", "A",  0,	  "BID",  TRUE,     12,   1,
     1,	"01/08/2017 09:00",  "A",	100,	"BID",  FALSE,    12,   1,
     1,	"01/11/2017 09:00",  "A",	100,	"BID",  FALSE,    12,   1,
     1,	"01/11/2017 22:00", "A",	100,	"BID",  FALSE,    12,   1
   ) %>% dplyr::mutate(
     TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
   )
   output <- tibble::tribble(
     ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,  ~II, ~EVID, ~ADDL,
     1,	"01/01/2017 10:00",	"A",	100,	"BID",  FALSE,     0,   1,     0,
     1,	"01/01/2017 21:00",	"A",	100,	"BID",  FALSE,    12,   1,     11,
     1,	"01/07/2017 22:00", "A",  0,	  "BID",  TRUE,      0,   1,     0,
     1,	"01/08/2017 09:00",  "A",	100,	"BID",  FALSE,     0,   1,     0,
     1,	"01/08/2017 21:00", "A",  100,  "BID",  FALSE,    12,   1,     4,
     1,	"01/11/2017 09:00",  "A",	100,	"BID",  FALSE,     0,   1,     0,
     1,	"01/11/2017 22:00", "A",	100,	"BID",  FALSE,     0,   1,     0
   ) %>% dplyr::mutate(
     TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
   )
   expect_equal(get_addl_dosing(input), output)
 })

 it("works for BID where no future doses are in the next SEQ", {
    input <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,     ~II, ~EVID,
      1,	"01/01/2017 10:00",	"A",	100,	"BID",  FALSE,    12,   1,
      1,	"01/07/2017 22:00", "A",  0,	  "BID",  TRUE,     12,   1,
      1,	"01/11/2017 10:00", "A",	100,	"BID",  FALSE,    12,   1,
      1,	"01/11/2017 22:00", "A",	100,	"BID",  FALSE,    12,   1
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    output <- tibble::tribble(
      ~ID,	~TIME,	         ~TRT, ~AMT,	~FREQ, ~MEX,     ~II, ~EVID, ~ADDL,
      1,	"01/01/2017 10:00",	"A",	100,	"BID",  FALSE,      0,   1,     0,
      1,	"01/01/2017 22:00",	"A",	100,	"BID",  FALSE,     12,   1,     11,
      1,	"01/07/2017 22:00", "A",  0,	  "BID",  TRUE,       0,   1,     0,
      1,	"01/08/2017 10:00", "A",  100,  "BID",  FALSE,     12,   1,     5,
      1,	"01/11/2017 10:00", "A",	100,	"BID",  FALSE,      0,   1,     0,
      1,	"01/11/2017 22:00", "A",	100,	"BID",  FALSE,      0,   1,     0
    ) %>% dplyr::mutate(
      TIME = as.POSIXct(anytime::anytime(TIME, tz = "UTC", asUTC = TRUE), origin = "1970-01-01")
    )
    expect_equal(get_addl_dosing(input), output)
  })
})
