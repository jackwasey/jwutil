#' Drop fields with duplicate data
#'
#' Compares all data in each field to every other field, and drops the latter
#' match. Will find multiple matches. Doesn't do any type conversions yet. This
#' is purely by content, not by field name.
#' @param df data.frame
#' @template verbose
#' @return data frame without duplicate fields
#' @examples
#' d <- data.frame(LETTERS, letters, letters)[1:10, ]
#' drop_duplicate_fields(d)
#' @export
drop_duplicate_fields <- function(df, verbose = FALSE) {
  stopifnot(is.logical(verbose), length(verbose) == 1)
  drop <- c()
  for (f in 1:(ncol(df) - 1)) {
    for (g in (f + 1):ncol(df)) {
      if (identical(all.equal(df[[f]], df[[g]]), TRUE)) {
        if (verbose) {
          message(sprintf(
            "found matching fields %s and %s. Dropping one.",
            names(df)[f], names(df)[g]
          ))
        }
        drop <- c(drop, names(df)[g])
      }
    }
  }
  for (dn in drop) df[dn] <- NULL
  df
}

#' @describeIn drop_duplicate_fields Deprecated
#' @export
dropDuplicateFields <- drop_duplicate_fields
