#' Day 03: Mull It Over
#'
#' [Mull It Over](https://adventofcode.com/2024/day/3)
#'
#' @name day03
#' @rdname day03
#' @details
#'
#' **Part One**
#'
#' Extract mul(x,y) instructions, run them, sum them.
#'
#' **Part Two**
#'
#' Extract mul(x,y) instructions, run them, sum them, but ignore any mul()s
#' between don't() and do() statements.
#'
#' @param x some data
#' @return For Part One, `f03a_compute_muls(x)` returns the sum of mul()
#'   statements. For Part Two, `f03b_compute_conditional_muls(x)` returns the
#'   sum of mul() statements.
#' @export
#' @examples
#' f03a_compute_muls(example_data_03())
#' f03b_compute_conditional_muls(example_data_03(2))
f03a_compute_muls <- function(x) {
  pattern <- "mul[(]\\d+,\\d+[)]"
  matches <- regmatches(x, gregexpr(pattern, x)) |> unlist()
  mul <- function(x, y) x * y
  matches |>
    lapply(function(x) eval(parse(text = x))) |>
    unlist() |>
    sum()
}


#' @rdname day03
#' @export
f03b_compute_conditional_muls <- function(x) {
  pattern <- "(mul[(]\\d+,\\d+[)])|(do[(][)])|(don't[(][)])"
  matches <- regmatches(x, gregexpr(pattern, x)) |> unlist()

  mul_active <- TRUE
  for (i in seq_along(matches)) {
    if (matches[i] == "don't()") mul_active <- FALSE
    if (matches[i] == "do()") mul_active <- TRUE
    if (!mul_active) matches[i] <- 0
  }
  do <- function() 0
  mul <- function(x, y) x * y

  matches |>
    lapply(function(x) eval(parse(text = x))) |>
    unlist() |>
    sum()
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day03
#' @export
example_data_03 <- function(example = 1) {
  l <- list(
    a = c(
      "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    ),
    b = c(
      "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    )
  )
  l[[example]]
}
