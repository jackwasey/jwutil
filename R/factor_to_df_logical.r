#' Convert factor into a data.frame of logicals
#'
#' Convert a single factor into a data.frame with multiple true or false fields,
#' one for each factor
#' @param fctr factor
#' @param prefix defaults to "f" to pre-pend the factor level when constructing
#'   the data frame columns names
#' @param sep scalar character, introduced between factor names and levels when
#'   forming new data frame column names
#' @param drop_empty logical, if `TRUE` (the default) factor levels with no
#'   associated values are dropped.
#' @param na_as_col logical scalar: if NA data and/or NA levels, then covert to
#'   NA strings and expand these as for any other factor
#' @template verbose
#' @return data.frame with columns of logicals
#' @examples
#' n <- 10
#' m <- 20
#' l <- LETTERS[seq_len(n)]
#' set.seed(1441)
#' f <- factor(sample(l, m, replace = T), levels = l)
#' g <- factor_to_df(f, drop_empty = FALSE)
#' print(g)
#' stopifnot(nrow(g) == m, ncol(g) == n)
#' factor_to_df_logical(
#'   shuffle(factor(shuffle(LETTERS[1:10]))),
#'   prefix = "")
#' factor_to_df(factor(c(NA, 1, 2, 3)))
#' factor_to_df(factor(c(NA, 1, 2, 3)), na_as_col = FALSE)
#' @export
factor_to_df <- function(fctr, prefix = deparse(substitute(fctr)),
                         sep = "", drop_empty = TRUE,
                         na_as_col = TRUE, verbose = FALSE) {
  stopifnot(is.factor(fctr))
  stopifnot(is.character(prefix) && length(prefix) == 1L)
  stopifnot(is.character(sep) && length(sep) == 1L)
  stopifnot(is.logical(na_as_col) && length(na_as_col) == 1)
  stopifnot(is.logical(verbose) && length(verbose) == 1)
  if (verbose && sum(is.na(fctr)) > 0)
    warning("factorToCols: factor passed to factorCols contains NA")
  if (drop_empty)
    fctr <- factor(fctr)
  stopifnot(length(levels(fctr)) > 0)
  stopifnot(length(fctr) > 0)
  if (na_as_col) {
    if (drop_empty)
      fctr <- factor(fctr, unique(fctr), exclude = NULL)
    else {
      new_levels <- unique(levels(fctr))
      if (anyNA(fctr))
        new_levels <- c(new_levels, NA)
      fctr <- factor(fctr, new_levels, exclude = NULL)
    }
    levels(fctr)[is.na(levels(fctr))] <- "NA"
  }
  if (length(levels(fctr)) == 1) {
    if (verbose) message("only one factor level, returning all TRUE")
    df <- data.frame(fctr)
    names(df) <- prefix
    return(df)
  }
  if (length(levels(fctr)) == 2) {
    if (verbose)
      message("two factor levels: returning TRUE/FALSE for first level")
    df <- data.frame(fctr == levels(fctr)[1])
    names(df) <- paste(prefix, levels(fctr)[1], sep = sep)
    return(df)
  }
  df <- data.frame(tmp = logical(length = length(fctr)))
  if (verbose)
    message("more than two factor levels")
  for (lev in levels(fctr)) {
    newColName <- paste(prefix, lev, sep = sep)
    if (verbose) message(sprintf("creating new column name: %s", newColName))
    df[newColName] <- fctr == lev
  }
  df["tmp"] <- NULL
  df
}

#' @rdname factor_to_df
#' @export
factorToDataframeLogical <- factor_to_df
