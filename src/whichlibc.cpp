#include <Rcpp.h>

// [[Rcpp::export]]
void check_standard() {
#ifdef __GLIBCXX__
  Rcpp::Rcout << "Using libstdc++ standard (associated w/ gcc)"  << std::endl;
#endif

#ifdef _LIBCPP_VERSION
  Rcpp::Rcout << "Using libc++  standard (clang origins)" << std::endl;
#endif
}

/***R
check_standard()
*/
