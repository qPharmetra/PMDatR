context("unit-cols")

describe("unit_cols()", {
  it("detects units if they exist", {
     th1 <- set_units_from_list(Theoph, list(conc = "mg/L", Wt = "kg"))
     expect_equal(unit_cols(th1), c(Wt = "kg", conc = "mg/L"))
  })
  it("gives null if no unit cols present", {
     expect_null(unit_cols(Theoph))
  })

})
