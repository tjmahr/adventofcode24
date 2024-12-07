library(adventofcode24)
x <- readLines("./inst/input07.txt")

p1 <- f07a_add_mul(x)
print(p1, digits = 15)
p2 <- f07b_add_mul_concat(x)
print(p2, digits = 15)

stopifnot(p1 == aoc_solutions$day07a)
stopifnot(p2 == aoc_solutions$day07b)
