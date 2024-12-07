test_that("day 07", {
  example_data_07() |>
    f07a_add_mul() |>
    expect_equal(3749)

  example_data_07() |>
    f07b_add_mul_concat() |>
    expect_equal(11387)
})
