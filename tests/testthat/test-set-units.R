context("set-units")

test_that("units can be set", {
  expect_equal(attr(set_units(10, "mg/L"), "pmunits"), "mg/L")
  #expect_error(set_units(10, "mg/hellothere"), 'units not parseable')
})

test_that("units set will not be lost on subsetting", {
  vec_units <- set_units(c(10, 10, 10, 10), "mg/L")
  Theoph$conc <- set_units(Theoph$conc, "mg/L")
  expect_equal(attr(vec_units, "pmunits"), "mg/L")
  expect_equal(attr(vec_units[1:3], "pmunits"), "mg/L")
  expect_equal(attr(Theoph[["conc"]], "pmunits"), "mg/L")
  expect_equal(attr(Theoph[["conc"]][1:3], "pmunits"), "mg/L")
})
