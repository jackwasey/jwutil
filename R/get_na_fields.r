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
#' @return vector of names of fields which contain any NA values, length zero if
#'   no matches
#' @export
getNAFields <- function(dframe) {
  stopifnot(is.data.frame(dframe))
  naFields <- names(dframe)[vapply(dframe, countIsNa, integer(1)) > 0]
  if (length(naFields)) naFields else character()
}

#' @rdname getNAFields
#' @export
getNonNAFields <- function(dframe)
  names(dframe)[names(dframe) %nin% getNAFields(dframe)]

#' @title return proportion of NA values per field
#' @description Return proportion of values which are \code{NA} in each field of
#'   the given data frame.
#' @param dframe is a data frame
#' @return numeric vector
#' @export
propNaPerField <- function(dframe)
  vapply(dframe, function(v) {
    countIsNa(v) / length(v)
  }, numeric(1))

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
dropRowsWithNAField <- function(x, fld = names(x), verbose = FALSE) {
  stopifnot(is.character(fld))
  stopifnot(is.data.frame(x))
  stopifnot(is.logical(verbose) && length(verbose == 1))
  if (verbose) message(sprintf("checking fields: %s for NA values",
                               paste(fld, sep = ", ")))
  cc <- stats::complete.cases(x[fld])
  droplevels(x[cc, ])
}

