library(adventofcode24)
x <- readLines("./inst/input01.txt")

p1 <- f01a_find_total_distance(x)
p2 <- f01b_compute_similarity(x)

stopifnot(p1 == aoc_solutions$day01a)
stopifnot(p2 == aoc_solutions$day01b)
