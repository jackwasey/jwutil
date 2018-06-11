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
strip_for_formula <- function(x) {
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
         Try trimming a column at a time.")
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
str_multi_match <- function(pattern, text, dropEmpty = FALSE, ...) {
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
  result[vapply(result, function(x) length(x) != 0), logical(1)]
}

#' @describeIn str_multi_match Deprecated
#' @export
strMultiMatch <- str_multi_match

#' Match pairs of strings to get named vector
#'
#' Match a character vector against a regular expression with at least two
#' parenthetic groupings, returning named vector.
#' @param string vector of strings
#' @param pattern vector of regular expression which should match exactly two
#'   strings for each element in \code{stringr}. If \code{pos} is specified,
#'   this rule is relaxed.
#' @param pos optional pair of integers with positions of the first and second
#'   desired matches, when multiple matches are made by the regular expression
#' @param swap logical scalar, whether to swap the names and values. Default is
#'   not to swap, so the first match becomes the name.
#' @keywords internal
str_pair_match <- function(string, pattern, pos, swap = FALSE, ...) {
  stopifnot(is.character(string) && length(string) >= 1L)
  stopifnot(is.logical(swap) && length(swap) == 1L)
  pos_missing <- missing(pos)
  if (pos_missing)
    pos <- c(1L, 2L)
  else
    stopifnot(length(pos) == 2L, min(pos) >= 1L, all(!is.na(pos)))
  res <- lapply(string,
                function(x) unlist(
                  regmatches(
                    x = x,
                    m = regexec(pattern = pattern, text = x, ...)
                  )
                )[-1]
  )
  res <- res[vapply(res, function(x) length(x) != 0, logical(1))]
  res <- do.call(rbind, res)
  if (pos_missing && ncol(res) > max(pos))
    stop("the pair matching has three or more ress but needed two.
         Use (?: to have a non-grouping regular expression parenthesis")
  out_names <- res[, ifelse(swap, 2L, 1L)]
  if (any(is.na(out_names)))
    stop("didn't match some rows:", string[is.na(out_names)],
         call. = FALSE)
  out <- res[, ifelse(swap, 1L, 2L)]
  stopifnot(all(!is.na(out)))
  setNames(out, out_names)
}

#' @describeIn str_pair_match Deprecated
#' @export
strPairMatch <- str_pair_match

#' mimic the \code{R CMD check} test
#'
#' \code{R CMD check} is quick to tell you where \code{UTF-8} characters are not
#' encoded, but gives no way of finding out which or where
#' @examples
#' \dontrun{
#' sapply(icd9cm_hierarchy, icd:::get_non_ASCII)
#' icd:::get_encodings(icd9cm_hierarchy)
#' sapply(icd9cm_billable, icd:::get_non_ASCII)
#' sapply(icd9cm_billable, icd:::get_encodings)
#' }
#' @keywords internal
get_non_ASCII <- function(x)
  x[is_non_ASCII(as_char_no_warn(x))]

#' @rdname get_non_ASCII
#' @keywords internal
is_non_ASCII <- function(x)
  is.na(iconv(as_char_no_warn(x), from = "latin1", to = "ASCII"))

#' return all matches for regular expression
#' @keywords internal manip
str_match_all <- function(string, pattern, ...) {
  string <- as.character(string)
  regmatches(x = string, m = regexec(pattern = pattern, text = string, ...))
}

#' \code{stringr} does this, but here we have a small amount of base R code
#' @keywords internal
str_extract <- function(string, pattern, ...) {
  vapply(regmatches(x = string, m = regexec(pattern = pattern, text = string, ...)),
         FUN = `[[`, 1, FUN.VALUE = character(1L))
}
