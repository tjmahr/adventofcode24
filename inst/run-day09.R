library(adventofcode24)
x <- readLines("./inst/input09.txt")

p1 <- f09a_compute_checksum(x)
print(p1, digits = 20)
p2 <- f09b_reorder_and_compute_checksum(x)
print(p2, digits = 20)


stopifnot(p1 == aoc_solutions$day09a)
stopifnot(p2 == aoc_solutions$day09b)
