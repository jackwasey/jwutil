#' Convert logical columns of data frame to 0s and 1s
#'
#' Encode TRUE as 1, and FALSE as 0 (integers)
#' @param x data frame which may contain logical fields
#' @return data frame without logical fields
#' @keywords manip
#' @examples
#' d <- data.frame(a = c(TRUE, FALSE, TRUE),
#'                 b = c(FALSE, TRUE, FALSE),
#'                 c = c(-1, 0, 1),
#'                 d = c("not", "logical", "values")
#'                 )
#' logical_to_binary(d)
#' @export
logical_to_binary <- function(x) {
  stopifnot(is.data.frame(x))
  if (any(dim(x) == 0))
    stop("got zero in at least one dimension in data frame. %d, %d",
         dim(x)[1], dim(x)[2])
  logical_fields <- names(x)[vapply(x, is.logical, logical(1))]
  if (is.na(logical_fields) || length(logical_fields) == 0) return(x)
  x[, logical_fields] <-
    vapply(X = x[, logical_fields],
           FUN = ifelse, 1L, 0L,
           FUN.VALUE = integer(nrow(x)))
  x
}

#' @rdname logical_to_binary
#' @export
logicalToBinary <- logical_to_binary
