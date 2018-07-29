#' Convert a number into rounded integer percentage string
#'
#' The number is converted into a percentage, then rounded.
#' @param x numeric
#' @examples
#' percentize(-1)
#' percentize(1L)
#' percentize(7.7)
#' percentize(0.01)
#' percentize(0.001)
#' @export
percentize <- function(x) {
  paste0(round(x * 100, 0), "%")
}

#' Print integers with percentage of total rounded to integer
#'
#' Intended for succinctly printing summary data in a scientific publication.
#' @param x numeric number
#' @param n numeric total
#' @param fmt `sprintf` format, default being `%d (%s)`
#' @examples
#' npc(1, 100)
#' npc(1, 1)
#' npc(2, 1)
#' npc(1.321, 7.7432)
#' npc(7239, 234897)
#' npc(-10, 1000)
#' @md
#' @export
npc <- function(x, n, fmt = "%d (%s)") {
  sprintf(fmt, as.integer(round(x)), percentize(x / n))
}
