#' @title filter data with diagnostics
#' @description applies an expression to a data frame, and gives information
#'   about the numbers of dropped rows.
#' @param x data frame
#' @param expr expression in the context of the data frame, i.e. the terms
#'   should be column names.
#' @param verbose logical default is TRUE
#' @export
#' @return filtered data frame
filter_better <- function(x, expr, verbose = TRUE) {
  sexpr <- substitute(expr)
  fn <- deparse(substitute(x))
  if (fn == ".") fn <- "" else fn <- paste("'", fn, "'")

  fltr <- eval(sexpr, x)
  stopifnot(is.logical(fltr))

  if (verbose) message(sprintf(
    "Filtering %s with expression '%s' drops %d (%2.1f%%) of %d",
    fn, deparse(sexpr),
    sum(!fltr), 100 * sum(!fltr) / length(fltr), length(fltr)
  ))
  x[fltr, ]
}

#' @describeIn filter_better Deprecated
#' @export
filterBetter <- filter_better
