library(adventofcode24)
x <- readLines("./inst/input11.txt")

p1 <- f11a_blink(x, 25)
p2 <- f11b_blink_75(x)
print(p2, digits = 20)

stopifnot(p1 == aoc_solutions$day11a)
stopifnot(p2 == aoc_solutions$day11b)
