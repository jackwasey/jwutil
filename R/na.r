# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of jwutil.
#
# jwutil is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# jwutil is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with jwutil If not, see <http:#www.gnu.org/licenses/>.

#' @title get NA field names from data frame
#' @description Get the names of any columns in a data frame which have NA
#'   values.
#' @param dframe data.frame
#' @param na_ish Logical, if `TRUE`, also consider NA-like strings, using
#'   `is_na_ish`
#' @param extra_na passed on to `is_na_ish`
#' @return vector of names of fields which contain any NA values, length zero if
#'   no matches
#' @export
get_na_fields <- function(dframe, na_ish = FALSE, extra_na = NULL) {
  stopifnot(is.data.frame(dframe))
  na_fields <- vapply(dframe, anyNA, logical(1))
  if (na_ish)
    na_fields <- na_fields | vapply(dframe,
                                    function(x) any(is_na_ish(x)),
                                    logical(1))
  names(dframe)[na_fields]
}

#' @describeIn get_na_fields Deprecated
#' @export
getNAFields <- get_na_fields

#' @rdname get_na_fields
#' @export
get_non_na_fields <- function(dframe)
  names(dframe)[names(dframe) %nin% getNAFields(dframe)]

#' @describeIn get_na_fields Deprecated
#' @export
getNonNAFields <- get_non_na_fields

#' @title return proportion of NA values per field
#' @description Return proportion of values which are \code{NA} in each field of
#'   the given data frame.
#' @param dframe is a data frame
#' @return numeric vector
#' @export
propNaPerField <- function(dframe)
  vapply(dframe, function(v) countIsNa(v) / length(v), numeric(1))

#' @title drops rows with NA values in specified fields
#' @description employs \code{\link[stats]{complete.cases}} which is fast
#'   internal C code. Returns a data frame with unused factor levels dropped
#'   (these may have been introduced by dropping rows with some NA values)
#' @param x data frame
#' @param fld vector with names of fields which must have no NA values
#' @template verbose
#' @return data frame without rows containing NA in the specified data fields.
#'   There may be NA values in the resulting data frame in fields which are not
#'   listed in fld.
#' @importFrom stats complete.cases
#' @export
 drop_rows_with_na <- function(x, fld = names(x), verbose = FALSE) {
  stopifnot(is.character(fld))
  stopifnot(is.data.frame(x))
  stopifnot(is.logical(verbose) && length(verbose == 1))
  if (verbose) message(sprintf("checking fields: %s for NA values",
                               paste(fld, sep = ", ")))
  cc <- stats::complete.cases(x[fld])
  base::droplevels(x[cc, ])
}

#' @describeIn drop_rows_with_na Deprecated, use `drop_rows_with_na`
#' @md
#' @export
dropRowsWithNAField <- drop_rows_with_na

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
#' d[1, 1] <- "NA"
#' zero_na(d, na_sih = TRUE)
#' @export
zero_na <- function(df, cols = names(df), ignore = character(), verbose = FALSE,
                    na_ish = TRUE) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(cols))
  stopifnot(is.character(ignore))
  stopifnot(is.logical(verbose))
  stopifnot(all(c(cols, ignore) %in% names(df)))
  stopifnot(is.logical(na_ish))
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

#' Determine whether a value is, or should be, `NA`
#' @param x vector to test
#' @param extra_na Additional values to consider equivalent to NA
#' @examples
#' is_na_ish(c(NA, "1"))
#' is_na_ish(c("NA", "N/A", "NaN"))
#' is_na_ish(c(NA))
#' is_na_ish(c(NA))
#' @export
is_na_ish <- function(x, extra_na = NULL) {
  stopifnot(is.atomic(x))
  na_equivs <- c("NA", "na", "N/A", "n/a", "NaN", "nan", "NAN", extra_na)
  is.na(x) |
    is.nan(x) |
    is.infinite(x) |
    is.null(x) |
    (trimws(x) %in% na_equivs)
}

#' Fix NA-like strings to be NA (or other value of choice)
#' @examples
#' df <- data.frame(a = c("NA", "n/a", 1, NA),
#'                  b = c("three", "na", NaN, "  N/A "),
#'                  stringsAsFactors = FALSE)
#' df
#' fix_na_ish(df)
#' fix_na_ish(df, extra_na = "three", new_val = "0")
#' @export
fix_na_ish <- function(dframe, extra_na = NULL, new_val = NA) {
  stopifnot(is.data.frame(dframe))
  for (col_name in names(dframe)) {
    col <- dframe[[col_name]]
    dframe[is_na_ish(x = col, extra_na = extra_na), col_name] <- new_val
  }
  dframe
}
