
#ifndef UTIL_H_
#define UTIL_H_

#include <vector>
#include <string>
#include <Rcpp.h>

typedef std::string Str;
typedef std::vector<Str> VecStr;

typedef std::vector<int> VecInt;

// SOMEDAY replace with char, but this stops Rcpp::export working
typedef std::vector<int> ComorbidOut;

typedef std::vector<VecStr> VecVecStr;
typedef std::vector<VecInt> VecVecInt;
typedef VecVecInt::size_type VecVecIntSz;

typedef Rcpp::CharacterVector CV;

std::vector<std::string> fastIntToStringStd(std::vector<int> x);
Rcpp::CharacterVector fastIntToStringRcpp(Rcpp::IntegerVector x);

#endif
