# jwutil

master:
[![CRAN status](https://www.r-pkg.org/badges/version/jwutil)](https://cran.r-project.org/package=jwutil)
[![Build Status](https://travis-ci.org/jackwasey/jwutil.png?branch=master)](https://travis-ci.org/jackwasey/jwutil) [![Coverage Status](https://img.shields.io/coveralls/jackwasey/jwutil.svg)](https://coveralls.io/r/jackwasey/jwutil?branch=master)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/jackwasey/jwutil?branch=master&svg=true)](https://ci.appveyor.com/project/jackwasey/jwutil)

This is a set of simple utilities for various data manipulation and caching tasks. The goal is to use base tools well, without bringing in any dependencies. Main areas of interest are data frame manipulation, such as converting factors in multiple binary indicator columns, and disk caching of data frames (which is optionally done by date range). There are testing functions which provide testthat extensions to permute arguments to function calls.

## install from CRAN
```
install.packages("jwutil")
```

## install from github
```
library(devtools)
install_github("jackwasey/jwutil")
```
