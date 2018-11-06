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

#' @title update a set of data frame field names
#' @description prefix or suffix
#' @param fields char vector
#' @param affix character
#' @param skip char vector, defaults to include all fields
#' @param renameHow should be "suffix" or "prefix", default is suffix
#' @param sep default "."
#' @return character vector, same length as fields
#' @export
affixFields <- function(fields, affix, skip = NULL,
                        renameHow = c("suffix", "prefix"),
                        sep = ".") {
  stopifnot(length(affix) == 1)
  stopifnot(nchar(affix) > 0)
  stopifnot(is.null(skip) || is.character(skip))
  renameHow <- match.arg(renameHow)
  if (renameHow == "suffix") {
    fields[fields %nin% skip] <-
      paste(fields[fields %nin% skip], affix, sep = sep)
  } else {
    fields[fields %nin% skip] <-
      paste(affix, fields[fields %nin% skip], sep = sep)
  }
  fields
}

#' @title get items or numerics that would be dropped in a merge
#' @description converts both vectors to numeric. This simulates merging when
#'   one key is character (but contains integer numbers), and another key is
#'   stored as integer.
#' @param x vector or factor
#' @param y vector or factor
#' @return list of two vectors
#' @export
getDropped <- function(x, y)
  list(
    missing_from_x = y[y %nin% x],
    missing_from_y = x[x %nin% y]
  )

#' @title names of fields which are numeric, binary or combinations thereof
#' @description Doesn't make any allowance for factors.
#' @param x data frame
#' @param invert single logical, if true, will return non-binary columns
#' @return vector of column names
#' @examples
#' dat <- data.frame(c("a", "b"), c(TRUE, FALSE), c(1, 0), c(1L, 0L),
#'                   c(1L, 2L), c(0.1, 0.2), c("9", "8"))
#' names(dat) <- c("char", "bin", "binfloat", "binint",
#'                 "int", "float", "charint")
#' binary_cols(dat)
#' binary_col_names(dat)
#' binary_col_names(dat, invert = TRUE)
#' @export
binary_col_names <- function(x, invert = FALSE) {
  stopifnot(is.data.frame(x))
  stopifnot(is.logical(invert), length(invert) == 1L)
  names(x)[xor(vapply(x, function(y) all(y %in% c(0, 1)), logical(1)), invert)]
}

#' @describeIn binary_col_names Get the columns which have exactly two
#'   categories therein, not including NA values. This would catch 0,1 "Yes",
#'   "No", etc.
#' @param ignore_na If TRUE, then return columns with two distinct values in
#'   addition to NA. Default is FALSE, i.e. NA is counted as a distinct item.
#' @param trim If character column found, then trim white space before assessing
#' @examples
#' df <- data.frame(x = c("A", "B", "A", "B"),
#'                  y = letters[1:4],
#'                  z = c("y", NA, "y", NA),
#'                  stringsAsFactors = FALSE)
#' two_cat_col_names(df)
#' df[1, 1] <- NA
#' df[2, 2] <- NA
#' df
#' stopifnot(two_cat_col_names(df) == "z")
#' stopifnot(two_cat_col_names(df, ignore_na = TRUE) == "x")
#' @export
two_cat_col_names <- function(dframe, invert = FALSE,
                              ignore_na = FALSE, trim = TRUE) {
  stopifnot(is.data.frame(dframe))
  stopifnot(is.logical(invert), length(invert) == 1L)
  is_two_cat <- vapply(dframe,
                       FUN = function(y) {
                         if (is.character(y)) y <- trimws(y)
                         length(unique(y)) == 2L + (anyNA(y) && ignore_na)
                         }, FUN.VALUE = logical(1))
  names(dframe)[xor(is_two_cat, invert)]
}

#' @describeIn binary_col_names Get the data frame containing just the binary
#'   columns.
#' @export
binary_cols <- function(x, invert = FALSE) {
  x[binary_col_names(x = x, invert = invert)]
}

#' @describeIn binary_col_names Get the data frame containing only columns of input which have
#'   two categories
#' @export
two_cat_cols <- function(x, invert = FALSE) {
  x[two_cat_col_names(x = x, invert = invert)]
}

#' @title fill out missing combinations of factors with NA
#' @description fill out missing combinations of factors with NA
#' @param df data frame
#' @details Adapated from
#'   \url{http://www.cookbook-r.com/Manipulating_data/Summarizing_data/#using-aggregate}
#' @export
fillMissingCombs <- function(df) {
  levelList <- list()
  for (f in getFactorNames(df)) levelList[[f]] <- levels(df[, f])
  merge(expand.grid(levelList), df, all.x = TRUE)
}

#' minimal basic pre-processing metrics
#' @param x data.frame input
#' @param df_list list of data frames
#' @export
jw_df_basics <- function(x, df_list) {
  stopifnot(xor(missing(x), missing(df_list)))
  if (!missing(x)) df_list <- list(x)
  stopifnot(is.list(df_list) && all(vapply(df_list, is.data.frame, logical(1))))
  out <- lapply(df_list, .jw_df_basics_impl)
  if (length(out) > 1)
    out
  else
    out[[1]]
}

.jw_df_basics_impl <- function(x) {
  stopifnot(is.data.frame(x))
  n <- nrow(x)
  cl <- lapply(x, class)
  f <- vapply(x, is.factor, logical(1))
  u <- vapply(x, function(y) length(unique(y)), integer(1))
  n_na <- colSums(is.na(x))
  suppressWarnings({
    n_neg <- colSums(x < 0)
    n_zero <- colSums(x == 0)
  })
  n_neg[f] <- NA
  n_zero[f] <- NA
  cbind(
    name = names(x),
    class = cl,
    typeof = lapply(x, typeof),
    n_na, p_na = n_na / n,
    n_neg, p_neg = n_neg / n,
    n_zero, p_zero = n_zero / n,
    n_uniq = u,
    p_uniq = u / n
  )
}
