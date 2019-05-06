#include <Rcpp.h>
using namespace Rcpp;

//' @name isRowSorted
//' @title is every row sorted?
//' @description Quicky run through rows of a matrix looking for any
//' non-ascending rows in C++
//' @param x matrix, each row containing ordered or disordered numerics
//' @export
// [[Rcpp::export]]
LogicalVector isRowSorted(NumericMatrix x) {
  const int nrow = x.nrow(), ncol = x.ncol();
  LogicalVector out(nrow, 1); // assume all are sorted to start with
  for (int i = 0; i < nrow; ++i) {
    for (int j = 1; j < ncol; ++j) {
      if (x(i,j-1) > x(i,j)) { out(i) = 0; break; }
    }
  }
  return out;
}

//' @name propRowSorted
//' @title proportion of non-descending rows in matrix
//' @description first performs isRowSorted to get a logical vector,
//' then sums TRUE values and takes fraction of total
//' @param x matrix, each row containing ordered or disordered numerics
//' @return double, the proportion from 0 to 1
//' @export
// [[Rcpp::export]]
double propRowSorted(NumericMatrix x) {
  LogicalVector lv = isRowSorted(x);
  return (double) sum(lv) / lv.size();
}
