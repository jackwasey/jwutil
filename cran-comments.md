## Update
Since last (failed) submission, 'testthat' quoted correctly in DESCRIPTION Description. Many thanks.

## Test environments
* Ubuntu 16.04 R 3.3.1, clang 3.9 trunk and gcc 5.3.1
* Ubuntu 12.04 (on travis-ci) R 3.3.0 patched, gcc 4.6.3
* OSX 10.11, clang 3.9

## R CMD check

CRAN repository db overrides:
  X-CRAN-Comment: Removed from CRAN on 2015-03-02 for policy violation.

This is now resolved by cleaning up temporary files during package tests.

There are no errors or warnings.
