#' Day 09: Disk Fragmenter
#'
#' [Disk Fragmenter](https://adventofcode.com/2024/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' A sequence of digits like `12345` is a disk map where each digit indicates
#' the size of a region on the disk. The map alternates between files and free
#' spaces. Each file is uniquely IDed by its position. So `12345` expands into
#' `0..111....22222`: a file of size 1, free space of size 2, etc.
#'
#' **Part One**
#'
#' Compact the disk by moving individual pieces of memory from the right end to
#' the leftmost free space. A compacted `12345` map yields `022111222......`.
#' Multiply each 0-based position by its ID value and take the sum.
#'
#' **Part Two**
#'
#' Compact the disk moving whole files by decreasing ID into the leftmost
#' region of contiguous free space that can fit the file. Multiply
#' each 0-based position by its ID value and take the sum.
#'
#' @param x some data
#' @return For Part One, `f09a_compute_checksum(x)` returns the checksum. For
#'   Part Two, `f09b_reorder_and_compute_checksum(x)` returns the checksum.
#' @export
#' @examples
#' f09a_compute_checksum(example_data_09())
#' f09b_reorder_and_compute_checksum(example_data_09())
f09a_compute_checksum <- function(x) {
  stream <- f09_helper(x)
  to_fill <- which(stream == ".")
  to_move <- which(stream != ".") |> rev() |> utils::head(length(to_fill))
  to_move <- to_move[to_move > to_fill]
  to_fill <- utils::head(to_fill, length(to_move))

  stream[to_fill] <- stream[to_move]
  stream[to_move] <- "."

  # -0 as a sentinel for dots that we need to treat as 0
  stream[stream == "."] <- "-0"
  positions <- seq_along(stream) - 1
  sum(positions * as.numeric(stream), na.rm = TRUE)
}


#' @rdname day09
#' @export
f09b_reorder_and_compute_checksum <- function(x) {
  split_by_run <- function(xs) {
    but_first <- xs[-1]
    but_final <- xs[-length(xs)]
    unname(split(xs, cumsum(c(TRUE, but_first != but_final))))
  }
  all_value <- function(l, value) {
    vapply(l, function(x) all(x == value), logical(1), USE.NAMES = FALSE)
  }
  swap_one <- function(l, id) {
    current_index <- which(all_value(l, id))
    slot <- l[[current_index]]

    dot_elements <- which(all_value(l, "."))
    # just empty areas with enough room
    dot_lengths <- lengths(l)[dot_elements]
    dot_elements <- dot_elements[dot_lengths >= length(slot)]
    # just empty areas to the left
    dot_elements <- dot_elements[dot_elements < current_index]

    if (length(dot_elements)) {
      target <- dot_elements[1]

      l[[target]][seq_along(slot)] <- id
      l[[current_index]][seq_along(slot)] <- "."

      # Naive approach
      # l <- l |>
      #   unlist(use.names = FALSE) |>
      #   split_by_run()

      # Attempt to speed-up by locally changing the list
      l <- c(
        l[seq_len(target - 1)],
        split_by_run(l[[target]]),
        l[seq(target + 1, length(l))]
      )
    }
    l
  }

  stream <- f09_helper(x)
  l <- split_by_run(stream)
  num_last_id <- stream[stream != "."] |>
    utils::tail(1) |>
    as.numeric()

  for (num_id in seq(num_last_id, 1, by = -1)) {
    l <- swap_one(l, as.character(num_id))
  }

  stream <- unlist(l, use.names = FALSE)
  stream[stream == "."] <- "-0"
  positions <- seq_along(stream) - 1
  sum(positions * as.numeric(stream), na.rm = TRUE)
}


f09_helper <- function(x) {
  vec_digits <- x |> strsplit("") |> unlist() |> as.numeric()
  list_digits <- as.list(vec_digits)

  odds <- seq(1, length(vec_digits), 2)
  ids <- seq_along(odds) - 1
  evens <- seq(2, length(vec_digits), 2)

  list_digits[odds] <- map2(ids, vec_digits[odds], rep)
  list_digits[evens] <- map2(".", vec_digits[evens], rep)
  stream <- unlist(list_digits)
  stream
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export
example_data_09 <- function(example = 1) {
  l <- list(
    a = c(
      "2333133121414131402"
    )
  )
  l[[example]]
}
