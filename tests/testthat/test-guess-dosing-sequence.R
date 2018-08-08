context("guess_dosing_sequence")

describe("guess_dosing_sequence works", {
  it("works with defaults", {
    expect_equal(
      guess_dosing_sequence(c(8, 13, 22), c(24, 24, 24)),
      c(1, 1, 1)
    )
    expect_equal(
      guess_dosing_sequence(c(8, 13, 22), c(12, 12, 12)),
      c(1, 2, 2)
    )
    expect_equal(
      guess_dosing_sequence(c(8, 13, 22), c(8, 8, 8)),
      c(1, 2, 3)
    )
  })
  it("works with overriding default", {
    # bid cutpoint of 10AM
    expect_equal(
      guess_dosing_sequence(c(8, 13, 22), c(12, 12, 12), list("BID" = 14)),
      c(1, 1, 2)
    )
  })
})
