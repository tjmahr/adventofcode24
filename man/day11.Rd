% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/day11.R
\name{day11}
\alias{day11}
\alias{f11a_blink}
\alias{f11b_blink_75}
\alias{example_data_11}
\title{Day 11: Plutonian Pebbles}
\usage{
f11a_blink(x, times)

f11b_blink_75(x)

example_data_11(example = 1)
}
\arguments{
\item{x}{some data}

\item{times}{number of times to blink}

\item{example}{Which example data to use (by position or name). Defaults to
1.}
}
\value{
For Part One, \code{f11a_blink(x)} returns the length after 25 iterations. For Part Two,
\code{f11b_blink_75(x)} returns the length after 75 iterations.
}
\description{
\href{https://adventofcode.com/2024/day/11}{Plutonian Pebbles}
}
\details{
Given a vector of numbers, apply the following rules: 1) replace 0 with 1, 2)
multiply numbers with an odd number of digits by 2024, and 3) split numbers
with an equal number of digits into 2 numbers with the same number of digits.

\strong{Part One}

How long is the output of this process after 25 iterations?

\strong{Part Two}

How long is the output of this process after 75 iterations?
}
\examples{
f11a_blink(example_data_11(), 6)
}
