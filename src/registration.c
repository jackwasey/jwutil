#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/*
  The following symbols/expressions for .NAME have been omitted

    _jwutil_set_attr_in_place
    _jwutil_getOmpMaxThreads
    _jwutil_getOmpThreads
    _jwutil_isRowSorted
    _jwutil_propRowSorted
    _jwutil_fastIntToStringStd
    _jwutil_fastIntToStringRcpp

  Most likely possible values need to be added below.
*/

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _jwutil_RcppExport_registerCCallable();
extern SEXP run_testthat_tests();

static const R_CallMethodDef CallEntries[] = {
    {"_jwutil_RcppExport_registerCCallable", (DL_FUNC) &_jwutil_RcppExport_registerCCallable, 0},
    {"run_testthat_tests",                   (DL_FUNC) &run_testthat_tests,                   0},
    {NULL, NULL, 0}
};

void R_init_jwutil(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
