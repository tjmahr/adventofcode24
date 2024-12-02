test_that("day 01", {
  example_data_01() |>
    f01a_find_total_distance() |>
    expect_equal(11)

  example_data_01() |>
    f01b_compute_similarity() |>
    expect_equal(31)
})
