#' Match across columns for multiple lookup values
#'
#' This provides a succinct way to query a data frame for conditions, which is
#' otherwise very verbose in base R or dplyr
#' @param x data.frame
#' @param cols character vector of column names to be found in `x`
#' @param table vector of items to find
#' @param incomparables passed on to the base function `match`
#' @return matrix with same number of rows as `x`, and a column for each of
#'   `cols`
#' @examples
#' j <- cars[1:10, ]
#' match_multi(j, "speed", 7)
#' match_multi(j, "dist", 22)
#' match_multi(j, c("speed", "dist"), 10)
#' match_multi(j, c("speed", "dist"), c(7, 17))
#' @md
#' @export
match_multi <- function(x, cols, table, incomparables = NULL) {
  vapply(
    x[cols],
    FUN = match,
    FUN.VALUE = integer(nrow(x)),
    table = table,
    nomatch = 0L,
    incomparables = incomparables
  ) != 0
}
