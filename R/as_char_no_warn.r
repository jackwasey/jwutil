#' convert to character vector without warning
#' @param x vector, typically numeric or a factor
#' @return character vector
#' @export
as_char_no_warn <- function(x) {
  if (is.character(x)) return(x)
  old <- options(warn = -1)
  on.exit(options(old))
  if (is.integer(x))
    fastIntToStringRcpp(x)
  if (is.factor(x))
    levels(x)[x]
  else
    as.character(x)
}
