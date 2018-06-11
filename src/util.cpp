#include "util.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' @name fastIntToString
//' @title Fast convert integer vector to character vector
//' @description Fast conversion from integer vector to character vector using C++
//' @param x vector of integers
//' @param bufferSize int if any input strings are longer than this number
//'   (default 16) there will be memory errors. No checks done for speed.
//' @examples
//' \dontrun{
//' pts <- generate_random_pts(1e7)
//' # conclusion: buffer size matters little (so default to be more generous),
//' # and Rcpp version fastest.
//' microbenchmark::microbenchmark(fastIntToStringStd(pts$visit_id, buffer = 8),
//'                                fastIntToStringStd(pts$visit_id, buffer = 16),
//'                                fastIntToStringStd(pts$visit_id, buffer = 64),
//'                                fastIntToStringRcpp(pts$visit_id, buffer = 8),
//'                                fastIntToStringRcpp(pts$visit_id, buffer = 16),
//'                                fastIntToStringRcpp(pts$visit_id, buffer = 64),
//'                                as.character(pts$visit_id),
//'                                as_char_no_warn(pts$visit_id), times = 5)
//' }
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> fastIntToStringStd(std::vector<int> x) {
  std::vector<std::string>::size_type len = x.size();
  std::vector<std::string> out(len);
  char buffer[64];
  for (std::vector<double>::size_type i = 0; i != len; ++i) {
    sprintf(buffer, "%u", x[i]);
    out[i] = buffer;
  }
  return out;
}

//' @title Convert integers to strings as quickly as possible
//' @description Have tried R, `sprintf` with \pkg{Rcpp} and C++ standard
//' library. Doesn't do bounds checking, but limited by length of integers.
//' @param x Vector of integers
//' @return Vector of characters
//' @md
//' @keywords internal manip
// [[Rcpp::export]]
Rcpp::CharacterVector fastIntToStringRcpp(Rcpp::IntegerVector x) {
  size_t len = x.size();
  Rcpp::CharacterVector out(len);
  char buffer[64];
  for (size_t i = 0; i != len; ++i) {
    sprintf(buffer, "%u", x[i]);
    out[i] = buffer;
  }
  return out;
}

// don't forget to do this! # tools::package_native_routine_registration_skeleton(".", "src/registration.c", character_only = FALSE)
// ###  the backticks may screw up the tool so could do character_only FALSE! No idea why Rcpp does this, or why it seems to work on 'icd' anyway.
