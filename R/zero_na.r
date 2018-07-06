#' Zero NA values in a data.frame
#'
#' Zero NA values in a data.frame, including \code{cols} and exluding
#' \code{ignore}. Also does not replace \code{Date} or \code{POSIXt} fields.
#' @param df data.frame
#' @param cols names of columns to work on, default is all columns
#' @param ignore character vector of columns names to ignore
#' @param verbose TRUE or FALSE
#' @examples
#' d <- data.frame(1:5, 6:10, 11:15)
#' d[2, 3] <- NA
#' d[5, 2] <- NA
#' d[1, 1] <- NA
#' print(d)
#' zero_na(d)
#' @export
zero_na <- function(df, cols = names(df), ignore = character(), verbose = FALSE) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(cols))
  stopifnot(is.character(ignore))
  stopifnot(is.logical(verbose))
  stopifnot(all(c(cols, ignore) %in% names(df)))
  gotNA <- getNAFields(df)
  for (n in gotNA[gotNA %nin% ignore]) {
    x <- df[[n]]
    if (verbose)
      message(sprintf("zeroing NA values in %s", n))
    if (!methods::is(x, "POSIXt") && !is.Date(x) && !is.factor(x)) {
      df[is.na(x), n] <- 0
    } else if (verbose)
      message(sprintf("skipping factor or Date: %s", n))
  }
  df
}
