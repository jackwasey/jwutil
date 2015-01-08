#include <Rcpp.h>
#ifdef __linux__
#include <gperftools/profiler.h>
#endif
using namespace Rcpp;

// [[Rcpp::export]]
SEXP start_profiler(const CharacterVector x) {
  std::string y = std::string( x[0] );
  const char * fp = y.c_str();
  #ifdef __linux__
  ProfilerStart(fp);
  #endif
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP stop_profiler() {
  #ifdef __linux__
  ProfilerStop();
  #endif
  return R_NilValue;
}

//' @name stressprof
//' @title stressprof
//' @param x matrix, each row containing ordered or disordered numerics
//' @export
//' @import Rcpp
// [[Rcpp::export]]
LogicalVector stressprof(NumericMatrix x) {
  #ifdef __linux__
  ProfilerStart("/tmp/stressprof.log");
  #endif
  int nrow = x.nrow(), ncol = x.ncol();
  LogicalVector out(nrow, 1); // assume all are sorted to start with
  for (int k = 0; k < 10000; k++) {
    for (int i = 0; i < nrow; i++) {
      for (int j = 1; j < ncol; j++) {
        if (x(i,j-1) > x(i,j)) { out(i) = 0; break; }
      }
    }
  }
  #ifdef __linux__
  ProfilerStop();
  #endif
  return out;
}
