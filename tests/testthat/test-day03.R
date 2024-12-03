test_that("day 03", {
  example_data_03() |>
    f03a_compute_muls() |>
    expect_equal(161)

  example_data_03(2) |>
    f03b_compute_conditional_muls() |>
    expect_equal(48)
})
