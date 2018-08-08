context("resolve-units")

describe("resolve-units", {
  it("takes the first named unit", {
    expect_equal(
      resolve_units(
        list(
          c(conc = "mg/L"),
          c(conc = "ug/L")
          )
        ),
       c(conc = "mg/L")
      )
  })
  it("takes the first named unit for multiple vectors", {
    expect_equal(
      resolve_units(
        list(
          c(conc = "mg/L"),
          c(conc = "ug/L", dose = "mg"),
          c(conc = "ug/L", dose = "ug")
        )
      ),
      c(conc = "mg/L", dose = "mg")
      )
  })
  it("can handle a NULL element by skipping it", {
    expect_equal(
      resolve_units(
        list(
          NULL,
          c(conc = "mg/L"),
          c(conc = "ug/L", dose = "mg"),
          c(conc = "ug/L", dose = "ug")
        )
      ),
      c(conc = "mg/L", dose = "mg")
      )

  })
})
