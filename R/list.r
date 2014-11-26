#' @title merge lists by names
#' @description merge lists by combining all the elements of the list items with the matching names
#' @param x list with named elements
#' @param y list with named elements
#' @return list
#' @export
mergeLists <- function(x, y) {
  both <- list(x, y)
  n <- unique(unlist(lapply(both, names)))
  names(n) <- n
  lapply(n, function(ni) unlist(lapply(both, `[[`, ni)))
}

#' @title trim null or empty values from a list
#' @description delele null/empty entries in a list. Recursively looks through list if nested.
#' @param x list
#' @return trimmed list
#' @export
listTrim  <-  function(x){
  if (isFlat(x)) return(listTrimFlat(x))
  lapply(x, listTrim)
}

#' @title trim null or empty values from a list
#' @param x list
#' @return trimmed list
#' @export
listTrimFlat  <-  function(x) {   # delele null/empty entries in a list
  stopifnot(isFlat(x)) # inefficient to do this twice if called from listTrim, but hey ho.
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
#'   https://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion
#'
#' @export
flattenList <- function(..., na.rm = FALSE) {
  x <- list(...)
  y <- list()
  rapply(x, function(x) y <<- c(y,x))
  if (na.rm)
    return(y[!is.na(y)])
  y
}

#' @title determine whether a list is nested
#' @param x list
#' @return single logical
#' @export
isFlat <- function(x) {
  stopifnot(is.list(x))
  !any(lapply(x, class) == "list")
}
