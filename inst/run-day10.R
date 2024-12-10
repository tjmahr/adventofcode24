library(adventofcode24)
x <- readLines("./inst/input10.txt")

p1 <- f10a_score_trailheads(x)
p2 <- f10b_rate_trailheads(x)

stopifnot(p1 == aoc_solutions$day10a)
stopifnot(p2 == aoc_solutions$day10b)
