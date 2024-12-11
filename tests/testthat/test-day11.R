test_that("day 11", {
  example_data_11(2) |>
    f11a_blink(6) |>
    expect_equal(22)

  example_data_11(2) |>
    f11a_blink(25) |>
    expect_equal(55312)
})
