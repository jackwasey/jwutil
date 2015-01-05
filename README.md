# jwutil

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
