#' @title dput a testthat test
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
