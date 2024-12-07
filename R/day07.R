#' Day 07: Bridge Repair
#'
#' [Bridge Repair](https://adventofcode.com/2024/day/7)
#'
#' @name day07
#' @rdname day07
#' @details
#'
#' **Part One**
#'
#' Given a list of patterns like `X: A B C`, determine whether there is a way to
#' compute `X` through some combination of adding or multiplying elements in
#' `A`,`B`,`C`. In other words, solve whether `X == A ? B ? C` where `?` can be
#' `+` or `*`. Precedence is strictly left to right. Return sum of X values in
#' list that can be reconstructed in this way.
#'
#' **Part Two**
#'
#' `?` can also now be concatenation. Find the same sum.
#'
#' @param x some data
#' @return For Part One, `f07a_add_mul(x)` returns sum of elements that can
#'   reconstructed with multiplication or addition. For Part Two,
#'   `f07b_add_mul_concat(x)` returns sum of elements that can reconstructed
#'   with multiplication, addition or concatenation.
#' @export
#' @examples
#' f07a_add_mul(example_data_07())
#' f07b_add_mul_concat(example_data_07())
f07a_add_mul <- function(x) {
  crunch <- function(xs) {
    crunch_iter <- function(current, rest) {
      if (length(rest) == 0) {
        current
      } else {
        a <- current + rest[1]
        b <- current * rest[1]
        current <- c(a, b)
        rest <- rest[-1]
        Tailcall(crunch_iter, current, rest)
      }
    }
    crunch_iter(xs[1], xs[-1])
  }

  l <- f07_helper(x)
  l$parts |>
    lapply(crunch) |>
    map2(l$targets, function(x, y) y[is.element(y, x)]) |>
    unlist() |>
    sum()
}


#' @rdname day07
#' @export
f07b_add_mul_concat <- function(x) {
  crunch <- function(xs, target = NULL) {
    crunch_iter <- function(current, rest, target = NULL) {
      if (length(rest) == 0) {
        current
      } else {
        r <- rest[1]
        # avoid conversions
        concat <- current * 10 ^ nchar(r) + r
        current <- c(current + r, current * r, concat)
        # discard dead-ends
        if (!is.null(target)) {
          current <- current[current <= target]
        }
        rest <- rest[-1]
        Tailcall(crunch_iter, current, rest)
      }
    }
    crunch_iter(xs[1], xs[-1])
  }

  l <- f07_helper(x)
  l$parts |>
    map2(l$targets, crunch) |>
    map2(l$targets, function(x, y) y[is.element(y, x)]) |>
    unlist() |>
    sum()
}


f07_helper <- function(x) {
  # x <- example_data_07()
  numbers <- x |> strsplit(": | ") |> lapply(as.numeric)
  list(
    targets = lapply(numbers, utils::head, 1),
    parts = lapply(numbers, utils::tail, -1)
  )
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day07
#' @export
example_data_07 <- function(example = 1) {
  l <- list(
    a = c(
      "190: 10 19",
      "3267: 81 40 27",
      "83: 17 5",
      "156: 15 6",
      "7290: 6 8 6 15",
      "161011: 16 10 13",
      "192: 17 8 14",
      "21037: 9 7 18 13",
      "292: 11 6 16 20"
    )
  )
  l[[example]]
}
