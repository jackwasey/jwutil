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

#' dput a testthat test
#'
#' Generate an R expression containing a \code{testthat} expectation for the
#' given expression and its result. This is useful when you know that a certain
#' output is correct, and wish to generate a test case to reflect this.
#' @param ... expressions
#' @examples
#' dput_expect_equal("a" %nin% c("b", "c", "d"))
#' @return character vector with each element containing an R expression with
#'   \code{expect_equal} test case corresponding to the evaluated input
#'   expressions.
#' @export
dput_expect_equal <- function(...) {
  dots <- eval(substitute(alist(...)))
  for (to_eval in dots) {
    conn <- textConnection("res_str", "w", local = TRUE)
    res <- dput(to_eval, file = conn)
    close(conn)
    cat("expect_equal(")
    cat(deparse(substitute(to_eval)))
    cat(", ")
    cat(eval(res))
    cat(")\n")
  }
}
