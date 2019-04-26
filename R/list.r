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

#' merge lists by names
#'
#' merge lists by vector combining all the vector elements of the list items
#' with the matching names. Unnamed vectors in the list will be dropped
#' silently.
#' @param x unnested list with named elements, each of which is a vector
#' @param y unnested list with named elements, each of which is a vector
#' @return list of vectors
#' @export
mergeLists <- function(x, y) {
  stopifnot(isFlat(x), isFlat(y))
  stopifnot(all(vapply(x, is.vector, vector(length = 1))))
  stopifnot(all(vapply(y, is.vector, vector(length = 1))))
  both <- list(x, y)
  n <- unique(unlist(lapply(both, names)))
  names(n) <- n
  res <- lapply(n, function(ni) unlist(lapply(both, `[[`, ni)))
  res[!vapply(res, is.null, logical(1))]
}

#' @title trim null or empty values from a list
#' @description delele null/empty entries in a list. Recursively looks through
#'   list if nested.
#' @param x list
#' @return trimmed list
#' @export
listTrim <- function(x) {
  if (isFlat(x)) {
    listTrimFlat(x)
  } else {
    lapply(x, listTrim)
  }
}

#' @title trim null or empty values from a list
#' @description Trim \code{NULL} or empty values from a flat list.
#' @param x list
#' @return trimmed list
#' @export
listTrimFlat <- function(x) {
  # inefficient to do this twice if called from listTrim, but hey ho.
  stopifnot(isFlat(x))
  suppressWarnings(
    x[vapply(x, length, integer(1)) != 0 &
      !vapply(x, function(y) all(is.null(y)), logical(1)) &
      !vapply(x, function(y) all(is.na(y)), logical(1))]
  )
}

#' @title flatten a list
#' @description unlike unlist, this function returns a list of objects of
#'   different data types, but removes any depth
#' @param ... list or any set of objects which will be made into a list, may
#'   include lists and nested lists
#' @param na_rm will drop NA values if TRUE
#' @return list without nested lists, objects with preserved data types
#' @source
#'   https://stackoverflow.com/questions/8139677/\
#'   how-to-flatten-a-list-to-a-list-without-coercion
#' @export
flattenList <- function(..., na_rm = FALSE) {
  stopifnot(is.logical(na_rm) && length(na_rm) == 1L)
  x <- list(...)
  y <- list()
  rapply(x, function(x) y <- c(y, x))
  if (na_rm) return(y[!is.na(y)])
  y
}

#' @title determine whether a list is nested
#' @description Returns TRUE if the given list is not nested.
#' @param x list
#' @return single logical
#' @export
isFlat <- function(x) {
  stopifnot(is.list(x))
  !any(lapply(x, class) == "list")
}

#' Make a list using input argument names as names
#' @param ... arguments whose names become list item names, and whose values
#'   become the values in the list
#' @examples
#' a <- c(1, 2)
#' b <- c("c", "d")
#' stopifnot(
#'   identical(
#'     named_list(a, b),
#'     list(a = a, b = b)
#'   )
#' )
#' @export
list_named <- function(...) {
  x <- list(...)
  names(x) <- as.character(match.call()[-1])
  x
}
