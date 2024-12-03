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
#' "Our computers are having issues, so I have no idea if we have any Chief
#' Historians
#' <span title="There's a spot reserved for Chief Historians between the green toboggans and the red toboggans. They've never actually had any Chief Historians in stock, but it's best to be prepared.">in
#' stock</span>! You're welcome to check the warehouse, though," says the
#' mildly flustered shopkeeper at the [North Pole Toboggan Rental
#' Shop](/2020/day/2). The Historians head out to take a look.
#'
#' The shopkeeper turns to you. \"Any chance you can see why our computers
#' are having issues again?\"
#'
#' The computer appears to be trying to run a program, but its memory (your
#' puzzle input) is *corrupted*. All of the instructions have been jumbled
#' up!
#'
#' It seems like the goal of the program is just to *multiply some
#' numbers*. It does that with instructions like `mul(X,Y)`, where `X` and
#' `Y` are each 1-3 digit numbers. For instance, `mul(44,46)` multiplies
#' `44` by `46` to get a result of `2024`. Similarly, `mul(123,4)` would
#' multiply `123` by `4`.
#'
#' However, because the program\'s memory has been corrupted, there are
#' also many invalid characters that should be *ignored*, even if they look
#' like part of a `mul` instruction. Sequences like `mul(4*`, `mul(6,9!`,
#' `?(12,34)`, or `mul ( 2 , 4 )` do *nothing*.
#'
#' For example, consider the following section of corrupted memory:
#'
#'     xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
#'
#' Only the four highlighted sections are real `mul` instructions. Adding
#' up the result of each instruction produces *`161`*
#' (`2*4 + 5*5 + 11*8 + 8*5`).
#'
#' Scan the corrupted memory for uncorrupted `mul` instructions. *What do
#' you get if you add up all of the results of the multiplications?*
#'
#' **Part Two**
#'
#' As you scan through the corrupted memory, you notice that some of the
#' conditional statements are also still intact. If you handle some of the
#' uncorrupted conditional statements in the program, you might be able to
#' get an even more accurate result.
#'
#' There are two new instructions you\'ll need to handle:
#'
#' -   The `do()` instruction *enables* future `mul` instructions.
#' -   The `don't()` instruction *disables* future `mul` instructions.
#'
#' Only the *most recent* `do()` or `don't()` instruction applies. At the
#' beginning of the program, `mul` instructions are *enabled*.
#'
#' For example:
#'
#'     xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
#'
#' This corrupted memory is similar to the example from before, but this
#' time the `mul(5,5)` and `mul(11,8)` instructions are *disabled* because
#' there is a `don't()` instruction before them. The other `mul`
#' instructions function normally, including the one at the end that gets
#' re-*enabled* by a `do()` instruction.
#'
#' This time, the sum of the results is *`48`* (`2*4 + 8*5`).
#'
#' Handle the new instructions; *what do you get if you add up all of the
#' results of just the enabled multiplications?*
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
