test_that("day 05", {
  example_data_05() |>
    f05a_check_sequences_and_sum_midpoints() |>
    expect_equal(143)

  example_data_05() |>
    f05a_check_sequences_and_sum_midpoints() |>
    expect_equal(143)
})
