#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _jwutil_fastIntToStringRcpp(SEXP);
extern SEXP _jwutil_fastIntToStringStd(SEXP);
extern SEXP _jwutil_getOmpMaxThreads();
extern SEXP _jwutil_getOmpThreads();
extern SEXP _jwutil_isRowSorted(SEXP);
extern SEXP _jwutil_propRowSorted(SEXP);
extern SEXP _jwutil_RcppExport_registerCCallable();
extern SEXP _jwutil_set_attr_in_place(SEXP, SEXP, SEXP);
extern SEXP run_testthat_tests();

static const R_CallMethodDef CallEntries[] = {
  {"_jwutil_fastIntToStringRcpp",          (DL_FUNC) &_jwutil_fastIntToStringRcpp,          1},
  {"_jwutil_fastIntToStringStd",           (DL_FUNC) &_jwutil_fastIntToStringStd,           1},
  {"_jwutil_getOmpMaxThreads",             (DL_FUNC) &_jwutil_getOmpMaxThreads,             0},
  {"_jwutil_getOmpThreads",                (DL_FUNC) &_jwutil_getOmpThreads,                0},
  {"_jwutil_isRowSorted",                  (DL_FUNC) &_jwutil_isRowSorted,                  1},
  {"_jwutil_propRowSorted",                (DL_FUNC) &_jwutil_propRowSorted,                1},
  {"_jwutil_RcppExport_registerCCallable", (DL_FUNC) &_jwutil_RcppExport_registerCCallable, 0},
  {"_jwutil_set_attr_in_place",            (DL_FUNC) &_jwutil_set_attr_in_place,            3},
  {"run_testthat_tests",                   (DL_FUNC) &run_testthat_tests,                   0},
  {NULL, NULL, 0}
};

void R_init_jwutil(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
