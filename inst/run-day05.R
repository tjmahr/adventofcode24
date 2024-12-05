library(adventofcode24)
x <- readLines("./inst/input05.txt")

p1 <- f05a_check_sequences_and_sum_midpoints(x)
p2 <- f05b_repair_sequences_and_sum_midpoints(x)

stopifnot(p1 == aoc_solutions$day05a)
stopifnot(p2 == aoc_solutions$day05b)
