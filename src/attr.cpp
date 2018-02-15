#include <Rcpp.h>

// [[Rcpp::interfaces(r, cpp)]]

//' Set attribute on any SEXP in place
//'
//' @examples
//' \dontrun{
//' # benchmark to see whether setting an attribute on a function argument and returning it does a copy.
//' f <- function(x) { attr(x, "a") <- FALSE; x }
//' p <- generate_random_short_icd9(1)
//' q <- generate_random_short_icd9(1e6)
//' times <- 1e5
//' microbenchmark::microbenchmark(f(p), set_attr_in_place(p, "a", FALSE),
//'                                f(q), set_attr_in_place(q, "a", FALSE), times = times)
//' p2 <- p3 <- generate_random_short_icd9(1)
//' set_attr_in_place(p2, "a", FALSE)
//' stopifnot(identical(f(p3), p2))
//' # oh dear, \code{f} does copy.
//'
//' # see if we can return without deep copy using Rcpp:
//'
//' p4 <- p
//' q4 <- q
//' setDecimalCodeInPlace(p4)
//' stopifnot(identical(p4, setDecimalCode(p4)))
//' microbenchmark::microbenchmark(setDecimalCodeInPlace(p), setDecimalCode(p), times = times)
//' microbenchmark::microbenchmark(setDecimalCodeInPlace(q), setDecimalCode(q), times = times)
//'
//'
//' }
//' @param x Any R object for which an attribute can be set
//' @param name The name of the attribute, as length one character vector
//' @param value Value to be assigned to that attribute
//' @export
// [[Rcpp::export]]
void set_attr_in_place(Rcpp::RObject& x, Rcpp::String name, SEXP value) {
  x.attr(name) = value;
}
