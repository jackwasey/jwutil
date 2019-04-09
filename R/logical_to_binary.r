#' Convert logical columns of data frame to 0s and 1s
#'
#' Encode TRUE as 1, and FALSE as 0 (integers)
#' @param x data frame which may contain logical fields
#' @return data frame without logical fields
#' @keywords manip
#' @examples
#' d <- data.frame(
#'   a = c(TRUE, FALSE, TRUE),
#'   b = c(FALSE, TRUE, FALSE),
#'   c = c(-1, 0, 1),
#'   d = c("not", "logical", "values")
#' )
#' logical_to_binary(d)
#' @export
logical_to_binary <- function(x) {
  stopifnot(is.data.frame(x))
  if (any(dim(x) == 0)) {
    stop(
      "got zero in at least one dimension in data frame. %d, %d",
      dim(x)[1], dim(x)[2]
    )
  }
  logical_fields <- names(x)[vapply(x, is.logical, logical(1))]
  if (length(logical_fields) == 0) return(x)
  x[, logical_fields] <-
    vapply(
      X = x[, logical_fields],
      FUN = ifelse, 1L, 0L,
      FUN.VALUE = integer(nrow(x))
    )
  x
}

#' @rdname logical_to_binary
#' @export
logicalToBinary <- logical_to_binary

#' Take dataframe, and convert any columns with just two categories into logical
#'
#' E.g. "Yes" would be converted to TRUE, "0" to FALSE, etc. If heuristics fail,
#' then the function stops with an error message. `NA` values are counted,
#' unless `ignore_na` is `TRUE`. When they are considered, `na_val`
#' indicates whether they are attributed `TRUE` or `FALSE`.
#' @param x input data frame
#' @param ignore_na logical
#' @param na_val Single value to use in place of `NA``, default is `FALSE`
#' @return data frame with two categories columns replaced by logical columns
#' @md
#' @examples
#' df <- data.frame(
#'   a = c("y", "n", "y", "y", "n"),
#'   b = c(FALSE, TRUE, FALSE, TRUE, TRUE),
#'   c = c(NA, NA, NA, NA, NA),
#'   d = c(NA, "yes", NA, NA, "yes"),
#'   e = c("y ", "n ", NA, "y ", "n "),
#'   f = c("YES   ", "NO     ", "NO    ", " YES", " NO "),
#'   stringsAsFactors = FALSE
#' )
#' df
#' res <- two_cat_to_logical(df)
#' stopifnot(identical(res$a, c(TRUE, FALSE, TRUE, TRUE, FALSE)))
#' stopifnot(identical(res$b, c(FALSE, TRUE, FALSE, TRUE, TRUE)))
#' two_cat_to_logical(df, ignore_na = TRUE)
#' @export
two_cat_to_logical <- function(x, ignore_na = FALSE, na_val = FALSE) {
  tcn <- two_cat_col_names(x, ignore_na = ignore_na)
  nr <- nrow(x)
  true_vals <- list("Y", "y", "Yes", "YES", "yes", "1", 1L, 1.0, TRUE, -1L, -1.0)
  false_vals <- list("N", "n", "No", "NO", "no", "0", 0L, 0.0, FALSE)
  if (!ignore_na) {
    if (na_val) {
      true_vals <- c(true_vals, NA)
    } else {
      false_vals <- c(false_vals, NA)
    }
  }
  can_convert <- sapply(
    x[tcn],
    function(y)
      all(match(trimws(y), c(true_vals, false_vals)) > 0L, na.rm = TRUE)
  )
  if (!all(can_convert)) {
    message("Cannot convert the following columns:")
    print(names(can_convert)[!can_convert])
    stop(call. = FALSE)
  }
  for (col_name in tcn) {
    new_vec <- rep(TRUE, nr)
    new_vec[trimws(x[[col_name]]) %in% false_vals] <- FALSE
    x[[col_name]] <- new_vec
  }
  x
}
