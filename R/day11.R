#' Day 11: Plutonian Pebbles
#'
#' [Plutonian Pebbles](https://adventofcode.com/2024/day/11)
#'
#' @name day11
#' @rdname day11
#' @details
#'
#' Given a vector of numbers, apply the following rules: 1) replace 0 with 1, 2)
#' multiply numbers with an odd number of digits by 2024, and 3) split numbers
#' with an equal number of digits into 2 numbers with the same number of digits.
#'
#' **Part One**
#'
#' How long is the output of this process after 25 iterations?
#'
#' **Part Two**
#'
#' How long is the output of this process after 75 iterations?
#'
#' @param x some data
#' @param times number of times to blink
#' @return For Part One, `f11a_blink(x)` returns the length after 25 iterations. For Part Two,
#'   `f11b_blink_75(x)` returns the length after 75 iterations.
#' @export
#' @examples
#' f11a_blink(example_data_11(), 6)
f11a_blink <- function(x, times) {
  f <- f11b_create_memoized_blink(5)
  x |>
    f11_helper() |>
    f(times) |>
    length()
}

#' @rdname day11
#' @export
f11b_blink_75 <- function(x) {
  # to optimize this function,
  # - we run 25 blinks,
  # - get very many values (A)
  # - take unique values of A (B)
  # - blink each B 25 times
  # - repeat this process
  # - then count the lengths of the outputs and
  # - replace values with lengths and sum them
  # - repeat this process
  f <- f11b_create_memoized_blink(5)

  l <- x |>
    f11_helper() |>
    f(25)

  # next 25 blinks for each unique value
  bridge_1 <- unique(l) |>
    as.list() |>
    lapply(f, 25) |>
    stats::setNames(unique(l))

  l2 <- bridge_1 |>
    lapply(unique) |>
    unlist(use.names = FALSE) |>
    unique()

  # next 25 blinks for each unique value
  bridge_2 <- l2 |>
    as.list() |>
    lapply(f, 25) |>
    stats::setNames(l2)

  # Replace each value from 26-50 series with 51-75 lengths
  lookup_2 <- lengths(bridge_2)
  partial_lengths <- bridge_1 |>
    lapply(as.character) |>
    lapply(function(x) lookup_2[x]) |>
    # Sum to get 26-75 lengths
    lapply(sum) |>
    unlist()

  # Replace each value from 25 with 26-75 lengths
  sum(partial_lengths[as.character(l)])
}


# This function creates a memoized function for performing iterations
f11b_create_memoized_blink <- function(chunk_size = 5) {
  # Use chunked memoization: given a digit n, find output after 5 steps.
  # store this value so it can be looked up.
  MEMO <- utils::hashtab(type = "identical", 100)

  blink_once <- function(numbers) {
    num_digits <- function(xs) {
      ifelse(xs == 0, 1, floor(log10(xs) + 1))
    }
    even_num_digits <- function(xs) {
      ifelse(xs == 0, FALSE, floor(log10(xs) + 1) %% 2 == 0)
    }
    split_even_num <- function(x) {
      half_digits <- num_digits(x) / 2
      divisor <- 10 ^ half_digits
      c(x %/% divisor, x %% divisor)
    }

    f <- function(n) {
      if (n == 0) {
        1
      } else if (even_num_digits(n)) {
        split_even_num(n)
      } else {
        n * 2024
      }
    }

    numbers |> lapply(f) |> unlist()
  }

  blink_chunk_times <- function(n) {
    n_key <- n
    new_n <- utils::gethash(MEMO, n_key)
    if (is.null(new_n)) {
      for (i in seq_len(chunk_size)) n <- blink_once(n)
      new_n <- n
      utils::sethash(MEMO, n_key, new_n)
    }
    new_n
  }

  f <- function(n, times) {
    blink_iter <- function(n, times) {
      if (times == 0) {
        n
      } else if (times < chunk_size) {
        for (i in seq_len(times)) n <- blink_once(n)
        n
      } else {
        n <- n |> lapply(blink_chunk_times) |> unlist()
        times <- times - chunk_size
        Tailcall(blink_iter, n, times)
      }
    }
    blink_iter(n, times)
  }
  attr(f, "chunk_size") <- chunk_size
  attr(f, "MEMO") <- MEMO
  f
}


f11_helper <- function(x) {
  if (is.character(x)) {
    scan(text = x, quiet = TRUE)
  } else (
    x
  )
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day11
#' @export
example_data_11 <- function(example = 1) {
  l <- list(
    a = "0 1 10 99 999",
    b = "125 17"
  )
  l[[example]]
}
