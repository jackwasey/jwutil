
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
  if (length(naFields) == 0)
    character()
  else
    naFields
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
#' @description employs \code{stats::complete.cases} which is fast internal C
#'   code. Returns a data frame with unused factor levels dropped (these may
#'   have been introduced by dropping rows with some NA values)
#' @param x data frame
#' @param fld vector with names of fields which must have no NA values
#' @template verbose
#' @return data frame without rows containing NA in the specified data fields.
#'   There may be NA values in the resulting data frame in fields which are not
#'   listed in fld.
dropRowsWithNAField <- function(x, fld = names(x), verbose = FALSE) {
  if (verbose)
    message(sprintf("checking fields: %s for NA values",
                    paste(fld, sep = ", ")))
  stopifnot(is.character(fld))
  stopifnot(is.data.frame(x))
  cc <- stats::complete.cases(x[fld])
  droplevels(x[cc, ])
}

