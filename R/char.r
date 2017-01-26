# Copyright (C) 2014 - 2017  Jack O. Wasey
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

#' strip all whitespace
#'
#' could do this with regular expression, but slow, and this function is called
#' frequently. My only use case works with removal of all space character
#' whitespace, and I don't expect <TAB>. This uses non-unicode aware matching
#' for speed. This can be changed by setting useBytes to FALSE.
#'
#' \code{gsub} is probably quicker than \code{stringr}/\code{stringi}. For
#' comorbidity processing, this package prefers the faster \link{base}
#' functions, whereas \code{stringr} is used for tasks which are not time
#' critical, e.g. parsing source data to be included in the distributed
#' \code{icd} package.
#' @param x is a character vector to strip
#' @param pattern is the non-regex of the character to strip, default " "
#' @param useBytes logical scalar. Unlike gsub, this will default to TRUE here,
#'   therefore breaking unicode.
#' @return character vector
#' @export
#' @examples
#' \dontrun{
#' requireNamespace("microbenchmark")
#' requireNamespace("stringr")
#' x <- random_string(25000);
#' microbenchmark::microbenchmark(
#'   gsub(x = x, pattern = "A", replacement = "", fixed = TRUE, useBytes = TRUE),
#'   gsub(x = x, pattern = "A", replacement = "", fixed = TRUE, useBytes = TRUE, perl = TRUE),
#'   gsub(x = x, pattern = "A", replacement = ""),
#'   stringr::str_replace_all(x, "A", "")
#'   )
#' }
strip <- function(x, pattern = " ", useBytes = TRUE) {
  stopifnot(length(pattern) == 1)
  stopifnot(length(useBytes) == 1)
  stopifnot(is.character(pattern))
  stopifnot(is.logical(useBytes))
  gsub(pattern = pattern, replacement = "", x = x,
       fixed = TRUE, useBytes = useBytes)
}

#' @title strip a string so that it can be used as a variable name in a formula.
#' @description This excludes many symbols, so just strip all symbols leaving
#'   alphanumeric, and no whitespace.
#' @param x character vector of potential formula variables
#' @return character vector of length x
#' @export
stripForFormula <- function(x) {
  res <- gsub(pattern = "[^[:alnum:]]", replacement = "", x)
  stopifnot(anyDuplicated(res) == 0)
  res
}


#' @title strip whitespace from ends of each string in given character vector
#' @description slower than \code{strip}.
#' @param x is a character vector to trim
#' @return character vector
#' @export
trim <- function(x) {
  if (is.data.frame(x))
    stop("trimming data.frame gives unpredictable results.
         Try trimming a column at a time using [s]apply.")
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
          text = x, ...),
        ...)
    )[ -1]
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
  stopifnot(length(pattern) == 1)
  stopifnot(length(text) > 0)
  stopifnot(length(swap) == 1)
  stopifnot(length(dropEmpty) == 1)
  stopifnot(is.character(pattern))
  stopifnot(is.character(text))
  stopifnot(is.logical(swap))
  stopifnot(is.logical(dropEmpty))

  res <- strMultiMatch(pattern = pattern, text = text,
                       dropEmpty = dropEmpty, ...)
  stopifnot(all(sapply(res, function(x) length(x) == 2)))

  outNames <- vapply(X = res,
                     FUN = "[",
                     FUN.VALUE = character(1),
                     ifelse(swap, 2, 1))
  stopifnot(all(!is.na(outNames)))

  out <- vapply(X = res,
                FUN = "[",
                FUN.VALUE = character(1),
                ifelse(swap, 1, 2))
  stopifnot(all(!is.na(out)))

  names(out) <- outNames
  out
}
