#' Day 04: Ceres Search
#'
#' [Ceres Search](https://adventofcode.com/2024/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' \"Looks like the Chief\'s not here. Next!\" One of The Historians pulls
#' out a device and pushes the only button on it. After a brief flash, you
#' recognize the interior of the [Ceres monitoring station](/2019/day/10)!
#'
#' As the search for the Chief continues, a small Elf who lives on the
#' station tugs on your shirt; she\'d like to know if you could help her
#' with her *word search* (your puzzle input). She only has to find one
#' word: `XMAS`.
#'
#' This word search allows words to be horizontal, vertical, diagonal,
#' written backwards, or even overlapping other words. It\'s a little
#' unusual, though, as you don\'t merely need to find one instance of
#' `XMAS` - you need to find *all of them*. Here are a few ways `XMAS`
#' might appear, where irrelevant characters have been replaced with `.`:
#'
#'     ..X...
#'     .SAMX.
#'     .A..A.
#'     XMAS.S
#'     .X....
#'
#' The actual word search will be full of letters instead. For example:
#'
#'     MMMSXXMASM
#'     MSAMXMSMSA
#'     AMXSXMAAMM
#'     MSAMASMSMX
#'     XMASAMXAMM
#'     XXAMMXXAMA
#'     SMSMSASXSS
#'     SAXAMASAAA
#'     MAMMMXMMMM
#'     MXMXAXMASX
#'
#' In this word search, `XMAS` occurs a total of *`18`* times; here\'s the
#' same word search again, but where letters not involved in any `XMAS`
#' have been replaced with `.`:
#'
#'     ....XXMAS.
#'     .SAMXMS...
#'     ...S..A...
#'     ..A.A.MS.X
#'     XMASAMX.MM
#'     X.....XA.A
#'     S.S.S.S.SS
#'     .A.A.A.A.A
#'     ..M.M.M.MM
#'     .X.X.XMASX
#'
#' Take a look at the little Elf\'s word search. *How many times does
#' `XMAS` appear?*
#'
#' **Part Two**
#'
#' The Elf looks quizzically at you. Did you misunderstand the assignment?
#'
#' Looking for the instructions, you flip over the word search to find that
#' this isn\'t actually an *`XMAS`* puzzle; it\'s an
#' *`X-MAS`*
#' puzzle in which you\'re supposed to find two `MAS` in the shape of an
#' `X`. One way to achieve that is like this:
#'
#'     M.S
#'     .A.
#'     M.S
#'
#' Irrelevant characters have again been replaced with `.` in the above
#' diagram. Within the `X`, each `MAS` can be written forwards or
#' backwards.
#'
#' Here\'s the same example from before, but this time all of the `X-MAS`es
#' have been kept instead:
#'
#'     .M.S......
#'     ..A..MSMS.
#'     .M.S.MAA..
#'     ..A.ASMSM.
#'     .M.S.M....
#'     ..........
#'     S.S.S.S.S.
#'     .A.A.A.A..
#'     M.M.M.M.M.
#'     ..........
#'
#' In this example, an `X-MAS` appears *`9`* times.
#'
#' Flip the word search from the instructions back over to the word search
#' side and try again. *How many times does an `X-MAS` appear?*
#'
#' @param x some data
#' @return For Part One, `f04a_count_xmas(x)` returns the number of XMAS/SMAX in
#'   the word search. For Part Two, `f04b_count_mas_x(x)` returns the number of
#'   MAS/SAM xs in the grid.
#' @export
#' @examples
#' f04a_count_xmas(example_data_04())
#' f04b_count_mas_x(example_data_04())
f04a_count_xmas <- function(x) {
  x <- f04_helper(x)
  diag_indices_l <- row(x) - col(x)
  diag_indices_r <- row(x) + col(x)
  to_search <- c(
    x |> split(diag_indices_l),
    x |> split(diag_indices_r),
    x |> split(row(x)),
    x |> split(col(x))
  ) |>
    vapply(paste0, character(1), collapse = "")

  sum(c(
    str_count(to_search, "XMAS"),
    str_count(to_search, "SAMX")
  ))

}


#' @rdname day04
#' @export
f04b_count_mas_x <- function(x) {
  m <- f04_helper(x)
  # pad so we don't have to check boundaries
  m <- cbind("", rbind("", m, ""), "")
  candidates <- which(m == "A", arr.ind = TRUE)

  offsets_l <- matrix(c(-1, 0, 1, -1, 0,  1), ncol = 2)
  offsets_r <- matrix(c(-1, 0, 1,  1, 0, -1), ncol = 2)

  diag_l <- candidates |>
    split(row(candidates)) |>
    lapply(function(x)  t(t(offsets_l) + x)) |>
    # matrix subsetting
    lapply(function(x) m[x]) |>
    lapply(paste0, collapse = "") |>
    unlist()

  diag_r <- candidates |>
    split(row(candidates)) |>
    lapply(function(x)  t(t(offsets_r) + x)) |>
    lapply(function(x) m[x]) |>
    lapply(paste0, collapse = "") |>
    unlist()

  sum(
    is.element(diag_l, c("MAS", "SAM")) & is.element(diag_r, c("MAS", "SAM"))
  )
}


f04_helper <- function(x) {
  x |>
    strsplit("") |>
    do.call(rbind, args = _)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day04
#' @export
example_data_04 <- function(example = 1) {
  l <- list(
    a = c(
      "MMMSXXMASM",
      "MSAMXMSMSA",
      "AMXSXMAAMM",
      "MSAMASMSMX",
      "XMASAMXAMM",
      "XXAMMXXAMA",
      "SMSMSASXSS",
      "SAXAMASAAA",
      "MAMMMXMMMM",
      "MXMXAXMASX"
    )
  )
  l[[example]]
}
