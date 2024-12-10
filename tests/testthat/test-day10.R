test_that("day 10", {
  example_data_10(1) |> f10a_score_trailheads() |> expect_equal(2)
  example_data_10(2) |> f10a_score_trailheads() |> expect_equal(4)
  example_data_10(3) |> f10a_score_trailheads() |> expect_equal(3)
  example_data_10(4) |> f10a_score_trailheads() |> expect_equal(36)
  example_data_10(4) |> f10b_rate_trailheads()  |> expect_equal(81)
})
