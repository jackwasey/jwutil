#' @title get names of the factor fields in a data frame
#' @description Get the names of those fields in a data frame which are factors.
#' @param x data frame
#' @param consider character vector of field names of the data frame to test,
#'   default is to use all of them.
#' @return vector
#' @export
get_factor_fields <- function(x, consider = names(x)) {
  if (length(names(x)) <= 0 || length(consider) <= 0)
    return()

  consider[sapply(x[1, consider], is.factor)]
  # TODO if any duplicated
}

#' @rdname getFactorNames
#' @export
get_non_factor_fields <- function(x, consider = names(x)) {
  consider[consider %nin% getFactorNames(x, consider)]
}

#' @rdname get_factor_fields
#' @export
getFactorNames <- get_factor_fields

#' @rdname get_non_factor_fields
#' @export
getNonFactorNames <- get_non_factor_fields
