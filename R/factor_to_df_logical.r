#' @title convert factor into a data.frame of logicals
#' @description converts a single factor into a data.frame with multiple T/F
#'   fields, one for each factor
#' @param fctr factor
#' @param prefix defaults to "f" to pre-pend the factor level when constructing
#'   the data frame columns names
#' @param sep scalar character, introduced between factor names and levels when
#'   forming new data frame column names
#' @param na.rm logical scalar: if NA data and/or NA levels, then covert to NA
#'   strings and expand these as for any other factor
#' @template verbose
#' @return data.frame with columns of logicals
#' @export
factor_to_df_logical <- function(fctr,
                                 prefix = deparse(substitute(fctr)),
                                 sep = "",
                                 na.rm = TRUE,
                                 verbose = FALSE) {
  checkmate::assertFactor(fctr)
  checkmate::assertString(prefix)
  checkmate::assertString(sep)
  checkmate::assertFlag(na.rm)
  checkmate::assertFlag(verbose)
  if (verbose && sum(is.na(fctr)) > 0)
    warning("factorToCols: factor passed to factorCols contains NA")
  #remove unused factor levels
  fctr <- factor(fctr)
  stopifnot(length(levels(fctr)) > 0)
  stopifnot(length(fctr) > 0)
  if (na.rm) {
    # don't ignore NA values or levels
    fctr <- factor(fctr, unique(fctr), exclude = NULL)
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
  # set-up data frame with empty logical
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

#' @rdname factor_to_df_logical
#' @export
factorToDataframeLogical <- factor_to_df_logical
