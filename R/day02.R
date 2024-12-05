#' Day 02: Red-Nosed Reports
#'
#' [Red-Nosed Reports](https://adventofcode.com/2024/day/2)
#'
#' @name day02
#' @rdname day02
#' @details
#'
#' **Part One**
#'
#' Check whether lists of numbers are strictly increasing and differences less
#' than 3.
#'
#' **Part Two**
#'
#' Check how many lists satisfy the above requirement if one element can
#' be dropped/ignored from a list.
#'
#' @param x some data
#' @return For Part One, `f02a_count_safe_reports(x)` returns the number of safe
#'   reports. For Part Two, `f02b_count_safe_dampened_reports(x)` returns the
#'   number of safe reports if we discard one number from each report.
#' @export
#' @examples
#' f02a_count_safe_reports(example_data_02())
#' f02b_count_safe_dampened_reports(example_data_02())
f02a_count_safe_reports <- function(x) {
  x |>
    f02_helper() |>
    keep(f02_is_safe_report) |>
    length()
}

#' @rdname day02
#' @export
f02b_count_safe_dampened_reports <- function(x) {
  x |>
    f02_helper() |>
    lapply(function(a) {
      a |>
        utils::combn(length(a) - 1) |>
        apply(2, f02_is_safe_report) |>
        any()
      }
    ) |>
    unlist() |>
    sum()
}


f02_is_safe_report <- function(x) {
  is_monotonic(x, strictly = TRUE) && (max(abs(diff(x))) <= 3)
}

f02_helper <- function(x) {
  x |>
    lapply(function(a) scan(text = a, quiet = TRUE))
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day02
#' @export
example_data_02 <- function(example = 1) {
  l <- list(
    a = c(
      "7 6 4 2 1",
      "1 2 7 8 9",
      "9 7 6 2 1",
      "1 3 2 4 5",
      "8 6 4 4 1",
      "1 3 6 7 9"
    )
  )
  l[[example]]
}
