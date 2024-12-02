is_monotonic <- function(xs, strictly = FALSE) {
  is_increasing <- !is.unsorted(xs, strictly = strictly)
  is_decreasing <- !is.unsorted(-xs, strictly = strictly)
  is_increasing || is_decreasing
}
