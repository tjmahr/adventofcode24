#' Day 05: Print Queue
#'
#' [Print Queue](https://adventofcode.com/2024/day/5)
#'
#' @name day05
#' @rdname day05
#' @details
#'
#' **Part One**
#'
#' Check that lists of numbers (second half of input) are well-ordered according
#' to some rules (first half). Rules have form "X|Y" which means that X must
#' precede Y in an orderings with both. Return the sum of the middle number in
#' each well ordered list.
#'
#' **Part Two**
#'
#' Find a repair for the incorrectly-ordered lists and compute the sum.
#'
#' @param x some data
#' @return For Part One, `f05a_check_sequences_and_sum_midpoints(x)` returns the
#'   sums of middle element in each well-ordered list. For Part Two, `f05b(x)`
#'   returns fixes the lists that are not well-ordered and sums the middle
#'   elements.
#' @export
#' @examples
#' f05a_check_sequences_and_sum_midpoints(example_data_05())
#' f05b_repair_sequences_and_sum_midpoints(example_data_05())
f05a_check_sequences_and_sum_midpoints <- function(x) {
  l <- f05_helper(x)
  passing_sequences <- f05_evaluate_sequences(l)

  l$sequences[passing_sequences] |>
    lapply(function(x) x[stats::median(seq_along(x))]) |>
    unlist() |>
    as.numeric() |>
    sum()
}


#' @rdname day05
#' @export
f05b_repair_sequences_and_sum_midpoints <- function(x) {
  create_adjacency_matrix <- function(s, rules) {
    adjacency_matrix <- matrix(
      1,
      nrow = length(s),
      ncol = length(s),
      dimnames = list(s, s)
    )
    invalid_paths <- rules |>
      subset(a %in% s & b %in% s, select = c(b, a)) |>
      as.matrix()
    adjacency_matrix[invalid_paths] <- 0
    diag(adjacency_matrix) <- 0
    adjacency_matrix
  }

  repair_from_graph <- function(s, rules) {
    drop_with_rowname <- function(m, name) {
      m[! rownames(m) %in% name, , drop = FALSE]
    }

    drop_with_colname <- function(m, name) {
      m[, ! colnames(m) %in% name, drop = FALSE]
    }

    # Crucial idea: If there are no incoming paths into a node (row sum of 0),
    # then that node can be last
    adjacency_matrix <- create_adjacency_matrix(s, rules)
    ordering <- character(0)

    while (nrow(adjacency_matrix)) {
      can_drop <- names(which(rowSums(adjacency_matrix) == 0))
      adjacency_matrix <- adjacency_matrix |>
        drop_with_rowname(can_drop) |>
        drop_with_colname(can_drop)
      ordering <- c(can_drop, ordering)
    }
    ordering
  }

  l <- f05_helper(x)
  passing_sequences <- f05_evaluate_sequences(l)
  repaired <- l$sequences[!passing_sequences] |>
    lapply(repair_from_graph, l$rules)

  repaired |>
    lapply(function(x) x[stats::median(seq_along(x))]) |>
    unlist() |>
    as.numeric() |>
    sum()
}


f05_evaluate_sequences <- function(l) {
  evaluate_sequence <- function(s, list_pre, list_post) {
    steps <- seq_along(s) |>
      lapply(function(x) vec_split_at_index(s, x))

    # looping so we can quit as soon as a problem is found
    is_passing_sequence <- TRUE
    for (step_i in seq_along(steps)) {
      step <- steps[[step_i]]

      pre_in_post <- is.element(step$post, list_pre[[step$pivot]])
      # all(logical(0)) is TRUE so this works on empty vectors
      no_post_in_pre <- all(!pre_in_post)

      post_in_pre <- is.element(step$pre, list_post[[step$pivot]])
      no_post_in_pre <- all(!post_in_pre)

      is_passing_sequence <- no_post_in_pre && no_post_in_pre
      if (!is_passing_sequence) break
    }
    is_passing_sequence
  }

  r <- l$rules

  list_pre  <- r |> split(~b) |> lapply(getElement, "a")
  list_post <- r |> split(~a) |> lapply(getElement, "b")

  l$sequences |>
    lapply(evaluate_sequence, list_pre, list_post) |>
    unlist()
}


f05_helper <- function(x) {
  lines <- seq_along(x)
  rules <- x[lines < which(x == "")] |>
    lapply(strsplit, "[|]") |>
    lapply(unlist) |>
    lapply(function(x) data.frame(a = x[1], b = x[2])) |>
    do.call(what = rbind, args = _)

  sequences <- x[lines > which(x == "")] |>
    lapply(strsplit, ",") |>
    lapply(unlist)

  list(rules = rules, sequences = sequences)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day05
#' @export
example_data_05 <- function(example = 1) {
  l <- list(
    a = c(
      "47|53",
      "97|13",
      "97|61",
      "97|47",
      "75|29",
      "61|13",
      "75|53",
      "29|13",
      "97|29",
      "53|29",
      "61|53",
      "97|53",
      "61|29",
      "47|13",
      "75|47",
      "97|75",
      "47|61",
      "75|61",
      "47|29",
      "75|13",
      "53|13",
      "",
      "75,47,61,53,29",
      "97,61,53,29,13",
      "75,29,13",
      "75,97,47,61,53",
      "61,13,29",
      "97,13,75,29,47"
    )
  )
  l[[example]]
}
