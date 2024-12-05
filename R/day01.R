#' Day 01: Historian Hysteria
#'
#' [Historian Hysteria](https://adventofcode.com/2024/day/1)
#'
#' @name day01
#' @rdname day01
#' @details
#'
#' **Part One**
#'
#' Sort two columns, find total absolute difference in each row.
#'
#' **Part Two**
#'
#' Multiple each number in one column by its frequency in another column. Sum
#' these products.
#'
#' @param x puzzle input
#' @return For Part One, `f01a_find_total_distance(x)` returns the total
#'   distance between the sorted ids. For Part Two,
#'   `f01b_compute_similarity(x)` returns the total similarity score.
#' @export
#' @examples
#' f01a_find_total_distance(example_data_01())
#' f01b_compute_similarity(example_data_01())
f01a_find_total_distance <- function(x) {
  m <- f01_helper(x)
  m[, 1] <- sort(m[, 1])
  m[, 2] <- sort(m[, 2])
  (m[, 1] - m[, 2]) |>
    abs() |>
    sum()
}


#' @rdname day01
#' @export
f01b_compute_similarity <- function(x) {
  m <- f01_helper(x)
  nbins <- max(m)

  a <- tabulate(m[, 1], nbins)
  b <- tabulate(m[, 2], nbins)
  sum(a * b * seq_len(nbins))
}


f01_helper <- function(x) {
  x |>
    strsplit("\\s+") |>
    lapply(as.numeric) |>
    do.call(rbind, args = _)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day01
#' @export
example_data_01 <- function(example = 1) {
  l <- list(
    a = c(
      "3   4",
      "4   3",
      "2   5",
      "1   3",
      "3   9",
      "3   3"
    )
  )
  l[[example]]
}
