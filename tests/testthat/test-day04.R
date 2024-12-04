test_that("day 04", {
  example_data_04() |>
    f04a_count_xmas() |>
    expect_equal(18)

  example_data_04() |>
    f04b_count_mas_x() |>
    expect_equal(9)
})
