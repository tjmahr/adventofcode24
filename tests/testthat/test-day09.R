test_that("day 09", {
  example_data_09() |>
    f09a_compute_checksum() |>
    expect_equal(1928)

  example_data_09() |>
    f09b_reorder_and_compute_checksum() |>
    expect_equal(2858)
})
