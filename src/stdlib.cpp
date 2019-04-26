#include <Rcpp.h>

// these of course return the library used to compile jwutil dll only.

// [[Rcpp::export]]
bool stdlib_gnu() {
#ifdef __GLIBCXX__
  return true;
#endif
  return false;
}

// [[Rcpp::export]]
bool stdlib_llvm() {
#ifdef _LIBCPP_VERSION
  return true;
#endif
  return false;
}
