% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df.r
\name{binary_col_names}
\alias{binary_col_names}
\alias{two_cat_col_names}
\alias{binary_cols}
\alias{two_cat_cols}
\title{names of fields which are numeric, binary or combinations thereof}
\usage{
binary_col_names(x, invert = FALSE)

two_cat_col_names(x, invert = FALSE, ignore_na = FALSE, trim = TRUE)

binary_cols(x, invert = FALSE)

two_cat_cols(x, invert = FALSE)
}
\arguments{
\item{x}{data frame}

\item{invert}{single logical, if true, will return non-binary columns}

\item{ignore_na}{If TRUE, then return columns with two distinct values in
addition to NA. Default is FALSE, i.e. NA is counted as a distinct item.}

\item{trim}{If character column found, then trim white space before assessing}
}
\value{
vector of column names
}
\description{
Doesn't make any allowance for factors.
}
\section{Functions}{
\itemize{
\item \code{two_cat_col_names}: Get the columns which have exactly two
categories therein, not including NA values. This would catch 0,1 "Yes",
"No", etc.

\item \code{binary_cols}: Get the data frame containing just the binary
columns.

\item \code{two_cat_cols}: Get the data frame containing only columns of
input which have two categories
}}

\examples{
dat <- data.frame(
  c("a", "b"), c(TRUE, FALSE), c(1, 0), c(1L, 0L),
  c(1L, 2L), c(0.1, 0.2), c("9", "8")
)
names(dat) <- c(
  "char", "bin", "binfloat", "binint",
  "int", "float", "charint"
)
binary_cols(dat)
binary_col_names(dat)
binary_col_names(dat, invert = TRUE)
df <- data.frame(
  x = c("A", "B", "A", "B"),
  y = letters[1:4],
  z = c("y", NA, "y", NA),
  stringsAsFactors = FALSE
)
two_cat_col_names(df)
df[1, 1] <- NA
df[2, 2] <- NA
df
stopifnot(two_cat_col_names(df) == "z")
stopifnot(two_cat_col_names(df, ignore_na = TRUE) == "x")
}
