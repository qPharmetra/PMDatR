context("spread")

ex1 <- tibble::data_frame(moeity = rep(c("parent", "metabolite"), times = 2), OCC = rep(1:2, each = 2), DV = c(0.1, 0.2, 1.5, 2.5))
ex1_spread <- structure(list(OCC = 1:2, metabolite = c(0.2, 2.5), parent = c(0.1,
                       1.5)), .Names = c("OCC", "metabolite", "parent"), class = c("tbl_df",
                                                                       "tbl", "data.frame"), row.names = 1:2)
ex1_units <- ex1 %>%
  dplyr::mutate(units = rep(c("ug/L", "mg/L"), times = 2 ))

ex1_units_spread <- structure(list(OCC = 1:2, metabolite = structure(c(0.2, 2.5), pmunits = "mg/L", class = c("pmunits",
                                  "numeric")), parent = structure(c(0.1, 1.5), pmunits = "ug/L", class = c("pmunits",
                                   "numeric"))), .Names = c("OCC", "metabolite", "parent"), row.names = 1:2, class = c("tbl_df",
                                                                                                                       "tbl", "data.frame"))

describe("spread_", {
  it("works for non-units df", {
    expect_equal(spread_(ex1, "moeity", "DV"), ex1_spread)
  })
  it("works for df with units column", {
    expect_equal(spread_(ex1_units, "moeity", "DV", .units_col = "units"), ex1_units_spread)
  })
  it("works for df with units key", {
    expect_equal(spread_(ex1, "moeity", "DV", .units_key = list(parent = "ug/L", metabolite = "mg/L")), ex1_units_spread)
  })
  it("works for df with units column and key", {
    expect_equal(spread_(ex1_units, "moeity", "DV", .units_col = "units"), ex1_units_spread)
  })
  it("works for df with units column and key, resolving precedence", {
    expect_equal(unit_cols(spread_(ex1_units, "moeity", "DV", .units_col = "units",
                         .units_key = list(parent = "g/L", metabolite = "g/L"))),
                 c(metabolite = "mg/L", parent = "ug/L"))
  })
  it("works for df with units column and key, resolving precedence", {
    expect_equal(unit_cols(spread_(ex1_units, "moeity", "DV", .units_col = "units",
                         .units_key = list(parent = "g/L", metabolite = "g/L"),
                         .col_precedence = FALSE)), c(metabolite = "g/L", parent = "g/L"))
  })
})
