context("strip-units")

describe("strip_units()", {
  it("units are stripped from vector", {
    expect_equal(strip_units(set_units(10, "mg/L")), 10)
  })
})

describe("strip_units_df()", {
  it("units are stripped from all df", {
    expect_equal(
      strip_units_df(
       set_units_from_list(Theoph,
                         list(conc = "mg/L",
                              Wt = "kg"))
      )
      , Theoph)

  })
})
