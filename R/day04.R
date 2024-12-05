#' Day 04: Ceres Search
#'
#' [Ceres Search](https://adventofcode.com/2024/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' Find the word XMAS in a word search.
#'
#' **Part Two**
#'
#' Find cases in the word search where two copies of MAS form an X.
#'
#' @param x some data
#' @return For Part One, `f04a_count_xmas(x)` returns the number of XMAS/SMAX in
#'   the word search. For Part Two, `f04b_count_mas_x(x)` returns the number of
#'   MAS/SAM xs in the grid.
#' @export
#' @examples
#' f04a_count_xmas(example_data_04())
#' f04b_count_mas_x(example_data_04())
f04a_count_xmas <- function(x) {
  x <- f04_helper(x)
  diag_indices_l <- row(x) - col(x)
  diag_indices_r <- row(x) + col(x)
  to_search <- c(
    x |> split(diag_indices_l),
    x |> split(diag_indices_r),
    x |> split(row(x)),
    x |> split(col(x))
  ) |>
    vapply(paste0, character(1), collapse = "")

  sum(c(
    str_count(to_search, "XMAS"),
    str_count(to_search, "SAMX")
  ))

}


#' @rdname day04
#' @export
f04b_count_mas_x <- function(x) {
  m <- f04_helper(x)
  # pad so we don't have to check boundaries
  m <- cbind("", rbind("", m, ""), "")
  candidates <- which(m == "A", arr.ind = TRUE)

  offsets_l <- matrix(c(-1, 0, 1, -1, 0,  1), ncol = 2)
  offsets_r <- matrix(c(-1, 0, 1,  1, 0, -1), ncol = 2)

  diag_l <- candidates |>
    split(row(candidates)) |>
    lapply(function(x)  t(t(offsets_l) + x)) |>
    # matrix subsetting
    lapply(function(x) m[x]) |>
    lapply(paste0, collapse = "") |>
    unlist()

  diag_r <- candidates |>
    split(row(candidates)) |>
    lapply(function(x)  t(t(offsets_r) + x)) |>
    lapply(function(x) m[x]) |>
    lapply(paste0, collapse = "") |>
    unlist()

  sum(
    is.element(diag_l, c("MAS", "SAM")) & is.element(diag_r, c("MAS", "SAM"))
  )
}


f04_helper <- function(x) {
  x |>
    strsplit("") |>
    do.call(rbind, args = _)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day04
#' @export
example_data_04 <- function(example = 1) {
  l <- list(
    a = c(
      "MMMSXXMASM",
      "MSAMXMSMSA",
      "AMXSXMAAMM",
      "MSAMASMSMX",
      "XMASAMXAMM",
      "XXAMMXXAMA",
      "SMSMSASXSS",
      "SAXAMASAAA",
      "MAMMMXMMMM",
      "MXMXAXMASX"
    )
  )
  l[[example]]
}
