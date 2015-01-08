#include <Rcpp.h>
using namespace Rcpp;

//// [[Rcpp::export]]
//SEXP start_profiler(const CharacterVector x) {
//  std::string y = std::string( x[0] );
//  const char * fp = y.c_str();
//  #ifdef __linux__
//  ProfilerStart(fp);
//  #endif
//  return R_NilValue;
//}
//
//// [[Rcpp::export]]
//SEXP stop_profiler() {
//  #ifdef __linux__
//  ProfilerStop();
//  #endif
//  return R_NilValue;
//}
