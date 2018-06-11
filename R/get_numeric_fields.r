#' Find columns which are numeric
#'
#' Get field names, or the data itself, of fields in a data frame which are
#' numeric, or numeric-like characters.
#' @param x Data frame
#' @param invert Logical, if \code{FALSE} -- the default -- the numeric fields
#'   are returned, otherwise, non-numeric fields are returned.
#' @param attrition If less than this proportion of rows become \code{NA} on
#'   conversion to numeric, then accept this is a numeric column after all.
#' @export
get_numeric_char_field_names <- function(x, invert = FALSE, attrition = 0.05) {
  stopifnot(is.data.frame(x))
  stopifnot(is.logical(invert), length(invert) == 1L)
  stopifnot(is.numeric(attrition), length(attrition) == 1L)
  char_cols <- vapply(x, is.character, character(1L))
  was_na <- colSums(is.na(x[char_cols]))
  numberish <- colSums(is.na(asNumericNoWarn(x[char_cols])))
  new_na_ratio <- (numberish - was_na) / (nrow(x) - was_na)
  new_na_ratio
}

#' @rdname get_numeric_char_field_names
#' @export
get_numeric_field_names <- function(x, invert = FALSE) {
  stopifnot(is.data.frame(x))
  stopifnot(is.logical(invert), length(invert) == 1L)
  names(x)[vapply(x, FUN.VALUE = logical(1),
                  function(y) xor(is.numeric(y), invert))]
}

#' @rdname get_numeric_char_field_names
#' @export
get_numeric_fields <- function(x, invert = FALSE) {
  stopifnot(is.data.frame(x))
  stopifnot(is.logical(invert), length(invert) == 1L)
  x[get_numeric_field_names(x = x, invert = invert)]
}
