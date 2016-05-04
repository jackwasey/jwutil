## Test environments
* Ubuntu 16.04 R 3.3.0, clang 3.9 trunk and gcc 5.3.1
* Ubuntu 12.04 (on travis-ci) R 3.3.0 patched, gcc 4.6.3
* Wercker Debian (rocker/hadleyverse) GCC 5.3.1, R 3.2.4 patched

## R CMD check results

CRAN repository db overrides:
  X-CRAN-Comment: Removed from CRAN on 2015-03-02 for policy violation.

This is now resolved by cleaning up temporary files during package tests.
