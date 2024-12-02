test_that("day 02", {
  example_data_02() |>
    f02a_count_safe_reports() |>
    expect_equal(2)

  example_data_02() |>
    f02b_count_safe_dampened_reports() |>
    expect_equal(4)
})
