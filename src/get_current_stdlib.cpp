#include <Rcpp.h>

// Thanks to @coatless via
// https://github.com/RcppCore/Rcpp/issues/961

// [[Rcpp::export]]
Rcpp::CharacterVector get_cpplib() {
#ifdef __GLIBCXX__
  return("glibc++");
#endif

#ifdef _LIBCPP_VERSION
  return("libc++");
#endif

  return NA_STRING;
}
