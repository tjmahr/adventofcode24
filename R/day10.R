#' Day 10: Hoof It
#'
#' [Hoof It](https://adventofcode.com/2024/day/10)
#'
#' @name day10
#' @rdname day10
#' @details
#'
#' Given a topographic map (grid of numbers), a path on the map is a path from 0
#' to 1 to 2 to ... 9. A path can only move in orthogonal directions (up, left,
#' right, down).
#'
#' **Part One**
#'
#' Find the total number of 9s that are reachable from each 0. Sum the counts.
#'
#' **Part Two**
#'
#' Find the total number of unique paths from each 0 to all reachable 9s. Sum
#' the counts.
#'
#' @param x some data
#' @return For Part One, `f10a_score_trailheads(x)` returns the solution. For
#'   Part Two, `f10b_rate_trailheads(x)` returns the solution.
#' @export
#' @examples
#' f10a_score_trailheads(example_data_10(4))
#' f10b_rate_trailheads(example_data_10(4))
f10a_score_trailheads <- function(x) {
  as_sparse_matrix <- function(m) {
    Matrix::sparseMatrix(
      i = row(m)[m != 0],
      j = col(m)[m != 0],
      x = m[m != 0],
      dims = dim(m)
    )
  }

  l <- f10_helper(x)
  adj_m <- as_sparse_matrix(l$adjacency_matrix)
  paths <- adj_m %*% adj_m %*% adj_m %*%
    adj_m %*% adj_m %*% adj_m %*%
    adj_m %*% adj_m %*% adj_m

  l$trailhead_vecs |>
    lapply(function(vec) sum((paths %*% vec) > 0)) |>
    unlist() |>
    sum()
}

#' @rdname day10
#' @export
f10b_rate_trailheads <- function(x) {
  as_sparse_matrix <- function(m) {
    Matrix::sparseMatrix(
      i = row(m)[m != 0],
      j = col(m)[m != 0],
      x = m[m != 0],
      dims = dim(m)
    )
  }

  l <- f10_helper(x)
  adj_m <- as_sparse_matrix(l$adjacency_matrix)
  paths <- adj_m %*% adj_m %*% adj_m %*%
    adj_m %*% adj_m %*% adj_m %*%
    adj_m %*% adj_m %*% adj_m

  l$trailhead_vecs |>
    lapply(function(vec) sum(paths %*% vec)) |>
    unlist() |>
    sum()
}

f10_helper <- function(x) {
  parse_number <- function(xs, ignore = ".") {
    xs[xs %in% ignore] <- NA
    as.numeric(xs)
  }

  m <- x |>
    strsplit("") |>
    lapply(parse_number) |>
    do.call(rbind, args = _)

  l <- list()
  l$shift$l <- cbind(NA,      m[, -ncol(m)])
  l$shift$r <- cbind(m[, -1], NA)
  l$shift$u <- rbind(NA,      m[-nrow(m), ])
  l$shift$d <- rbind(m[-1, ], NA)

  l$to <- l$shift |>
    lapply(function(shift) m - shift == 1) |>
    lapply(which, arr.ind = TRUE)

  row_apply_if_length <- function(m, f, ...) {
    if (length(m)) {
      apply(m, 1, f, ...) |> t()
    } else {
      m
    }
  }
  l$from$u <- l$to$u |> row_apply_if_length(\(x) x + c(-1,  0))
  l$from$d <- l$to$d |> row_apply_if_length(\(x) x + c( 1,  0))
  l$from$l <- l$to$l |> row_apply_if_length(\(x) x + c( 0, -1))
  l$from$r <- l$to$r |> row_apply_if_length(\(x) x + c( 0,  1))

  adj_m <- matrix(0, nrow = length(m), ncol = length(m))

  adj_names <- paste(row(m), col(m))
  dimnames(adj_m) <- list(
    row = paste("r", adj_names),
    col = paste("c", adj_names)
  )

  # set_value_mijv <- function(m, i, j, v) { m[i, j] <- v; m }
  # set_value_mkv <- function(m, k, v) { m[k] <- v; m }
  set_value_mabv <- function(m, a, b, v) { m[cbind(a, b)] <- v; m }

  to_row_label <- function(xs) {
    xs |> apply(1, function(xs) paste(c("r", xs), collapse = " "))
  }
  to_col_label <- function(xs) {
    xs |> apply(1, function(xs) paste(c("c", xs), collapse = " "))
  }

  adj_m <- adj_m |>
    set_value_mabv(to_row_label(l$to$u), to_col_label(l$from$u), 1) |>
    set_value_mabv(to_row_label(l$to$d), to_col_label(l$from$d), 1) |>
    set_value_mabv(to_row_label(l$to$l), to_col_label(l$from$l), 1) |>
    set_value_mabv(to_row_label(l$to$r), to_col_label(l$from$r), 1)

  l <- list(
    matrix = m,
    adjacency_matrix = adj_m,
    trailhead_names = which(m == 0, arr.ind = TRUE) |>
      to_row_label()
  )

  l$trailhead_vecs <- l$trailhead_names |>
    lapply(function(x) { as.numeric(rownames(adj_m) == x)} )

  l
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day10
#' @export
example_data_10 <- function(example = 1) {
  l <- list(
    a = c(
      "...0...",
      "...1...",
      "...2...",
      "6543456",
      "7.....7",
      "8.....8",
      "9.....9"
    ),
    b = c(
      "..90..9",
      "...1.98",
      "...2..7",
      "6543456",
      "765.987",
      "876....",
      "987...."
    ),
    c = c(
      "10..9..",
      "2...8..",
      "3...7..",
      "4567654",
      "...8..3",
      "...9..2",
      ".....01"
    ),
    d = c(
      "89010123",
      "78121874",
      "87430965",
      "96549874",
      "45678903",
      "32019012",
      "01329801",
      "10456732"
    )
  )
  l[[example]]
}
