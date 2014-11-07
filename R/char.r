
#' @title strip all whitespace
#' @description could do this with regular expression, but slow, and this
#'   function is called frequently. My only use case works with removal of all
#'   space character whitespace, and I don't expect <TAB>.
#' @param x is a character vector to strip
#' @param pattern is the non-regex of the character to strip, default " "
#' @return character vector
#' @export
strip <- function (x, pattern = " ") {
  # beware unicode
  gsub(pattern = pattern, replacement = "", x, fixed = TRUE, useBytes = TRUE)
}

#' @title strip whitespace from ends of each string in given character vector
#' @description slower than \code{strip}.
#' @param x is a character vector to trim
#' @return character vector
#' @export
trim <- function(x) {
  if (class(x) == "data.frame") { stop("trimming data.frame gives unpredictable results.
                                       Try trimming a column at a time using [s]apply.")}
  gsub("^\\s+|\\s+$", "", x)
}

#' @title return the actual matches from a bracketed regex
#' @description Be careful: this may throw funny results for exotic regex, but
#'   so far, it seems okay. it also drops the first result which always seems to
#'   be a duplicate or whole-string match
#' @param pattern regular expression: if it has bracketed sections, these
#'   submatches are returned
#' @param text is the string to match against. This vector should be the same
#'   length as the pattern vector, or the patern vector should be length one.
#' @param ... are additional parameters passed to regexec and regmatches. I
#'   haven't tried this: it may need two separate variables containing lists of
#'   params, since this will send everything to both functions.
#' @param dropEmpty logical whether to drop rows with no matches
#' @return list of character vectors, list length being the length of the inptu
#'   text vector.
#' @export
strMultiMatch <- function(pattern, text, dropEmpty = FALSE, ...) {
  # unlist puts the name in the first position, which I don't think I ever want.
  result <- lapply(
    text, function(x) unlist(
      regmatches(
        x = x,
        m = regexec(
          pattern = pattern,
          text=x, ...),
        ...)
    )[-1]
  )
  if (!dropEmpty) return(result)
  result[sapply(result, function(x) length(x) != 0)]
}

#' @rdname strMultiMatch
#' @description \code{strPaitMatch} differs in that there should only be two
#'   pairs of parenthesis, then the first (by default) becomes the name, and the
#'   second the value.
#' @param swap logical scalar, whether to swap the names and values. Default is
#'   not to swap, so the first match becomes the name.
#' @export
strPairMatch <- function(pattern, text, swap = FALSE, dropEmpty = FALSE, ...) {
  res <- strMultiMatch(pattern = pattern, text = text, dropEmpty = TRUE, ...)
  outNames <- vapply(X = res, FUN = '[', FUN.VALUE = character(1), ifelse(swap, 2, 1))
  out <- vapply(X = res, FUN = '[', FUN.VALUE = character(1), ifelse(swap, 1, 2))
  names(out) <- outNames
  out
}
