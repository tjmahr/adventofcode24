library(adventofcode24)
x <- readLines("./inst/input02.txt")

p1 <- f02a_count_safe_reports(x)
p2 <- f02b_count_safe_dampened_reports(x)

stopifnot(p1 == aoc_solutions$day02a)
stopifnot(p2 == aoc_solutions$day02b)
