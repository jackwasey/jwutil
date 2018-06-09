#' Encode TRUE as 1, and FALSE as 0 (integers)
#'
#' When saving data as text files for distribution, printing large amounts of
#' text containing TRUE and FALSE is inefficient. Convert to binary takes more
#' \R memory, but allows more compact output
#' @param x dataframe which may contain logical fields
#' @return data frame without logical fields
#' @keywords manip
#' @export
logical_to_binary <- function(x) {
  stopifnot(is.data.frame(x))
  if (any(dim(x) == 0))
    stop("got zero in at least one dimension in data frame. %d, %d",
         dim(x)[1], dim(x)[2])

  # can condense this code into a one-liner, but this is clearer:
  logical_fields <- names(x)[sapply(x, is.logical)]
  if (is.na(logical_fields) || length(logical_fields) == 0) return(x)

  #update just the logical fields with integers
  x[, logical_fields] <-
    vapply(
      X         = x[, logical_fields],
      FUN       = function(y) ifelse(y, 1L, 0L),
      FUN.VALUE = integer(length = dim(x)[1])
    )
  x
}

#' @rdname logical_to_binary
#' @export
logicalToBinary <- logical_to_binary
