#' @title merge lists by names
#' @description merge lists by vector combining all the vector elements of the
#'   list items with the matching names. Unnamed vectors in the list will be
#'   dropped silently.
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
listTrim  <-  function(x){
  if (isFlat(x)) return(listTrimFlat(x))
  lapply(x, listTrim)
}

#' @title trim null or empty values from a list
#' @description Trim \code{NULL} or empty values from a flat list.
#' @param x list
#' @return trimmed list
#' @export
listTrimFlat  <-  function(x) {
  # inefficient to do this twice if called from listTrim, but hey ho.
  stopifnot(isFlat(x))
  suppressWarnings(
    x[sapply(x, length) != 0 &
        !sapply(x, function(y) all(is.null(y))) &
        !sapply(x, function(y) all(is.na(y)))
      ]
  )
}

#' @title flatten a list
#' @description unlike unlist, this function returns a list of objects of
#'   different data types, but removes any depth
#' @param ... list or any set of objects which will be made into a list, may
#'   include lists and nested lists
#' @param na.rm will drop NA values if TRUE
#' @return list without nested lists, objects with preserved data types
#' @source
#'   https://stackoverflow.com/questions/8139677/\
#'   how-to-flatten-a-list-to-a-list-without-coercion
#' @export
flattenList <- function(..., na.rm = FALSE) {
  x <- list(...)
  y <- list()
  rapply(x, function(x) y <- c(y, x))
  if (na.rm) return(y[!is.na(y)])
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
