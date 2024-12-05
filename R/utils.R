is_monotonic <- function(xs, strictly = FALSE) {
  is_increasing <- !is.unsorted(xs, strictly = strictly)
  is_decreasing <- !is.unsorted(-xs, strictly = strictly)
  is_increasing || is_decreasing
}

str_count <- function(x, pattern) {
  gregexpr(pattern = pattern, text = x) |>
    lapply(attr, "match.length") |>
    lapply(function(x) sum(x > 0)) |>
    unlist()
}

vec_split_at_index <- function(vec, i) {
  # subset() does the right thing on i == 1, split() does not
  s <- seq_along(vec)
  list(
    pre = subset(vec, s < i),
    pivot = subset(vec, s == i),
    post = subset(vec, s > i)
  )
}
