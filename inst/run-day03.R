library(adventofcode24)
x <- readLines("./inst/input03.txt")

p1 <- f03a_compute_muls(x)
p2 <- f03b_compute_conditional_muls(x)

stopifnot(p1 == aoc_solutions$day03a)
stopifnot(p2 == aoc_solutions$day03b)
