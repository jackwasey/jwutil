#' @title dput a testthat test
#' @description Generate an R expression containing a \code{testthat}
#'   expectation for the given expression and its result. This is useful when
#'   you know that a certain output is correct, and wish to generate a test case
#'   to reflect this.
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
    cat('expect_equal(')
    cat(deparse(substitute(to_eval)))
    cat(', ')
    cat(eval(res))
    cat(')\n')
  }
}
