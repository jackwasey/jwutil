# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of jwutil.
#
# jwutil is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# jwutil is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with jwutil If not, see <http:#www.gnu.org/licenses/>.

n_rnd <- 30

#' @title list all functions in a package
#' @description List functions in a package
#' @param pkg character string containing package name
#' @return character vector of functions in given package
#' @export
lsf <- function(pkg) {
  env_name <- paste("package", pkg, sep = ":")
  everything <- ls(pos = env_name, all.names = TRUE)
  funcs <- c()
  for (e in everything) {
    if (is.function(get(e, envir = as.environment(env_name), inherits = FALSE)))
      funcs <- append(funcs, e)
  }
  funcs
}

#' @title convert numbers to long and float types
#' @description intended for generating values for stress testing functions
#' @param ... list of values to convert to long and double
#' @param na.rm logical, defaults to TRUE, so output contains only long and
#'   float values.
#' @return list of long and double versions of convertable values from the input
#' @export
numbers_to_long_and_float <- function(..., na.rm = TRUE) {
  x <- flattenList(list(...))
  # drop any NA values. Very big numbers not representable by 32 bit integers,
  # give NA with warning. For test case generation, usually we will want to
  # remove NAs.
  suppressWarnings(flattenList(list(as.integer(x)),
                               list(as.double(x)),
                               na.rm = na.rm))
}

#' @title zeroes
#' @description long, float and complex types
#' @keywords sysdata
#' @export
zeroes <- list(0L, 0.0, complex(length.out = 1))

#' @title bad input data for tests
#' @description a variety of horrible data
#' @keywords sysdata
#' @export
bad_input <- c(
  list(c(), NA, list()),
  c(),
  list(list(1, 2), jack = "test", c(1, 2, 3), data.frame()),
  NULL,
  NA,
  this = "jack",
  data.frame(j = 1:10, k = 11:20),
  matrix(nrow = 2, ncol = 2),
  as.POSIXct(Sys.time(), "GMT"),
  base::.leap.seconds
)

#' @title create extreme random numbers
#' @description create random Dates, POSIX dates, letters and numbers. The
#'   numbers explore limits of R precision and floating point and integer
#'   ranges. Zero, negatives, positives.
#' @param n integer number of each group to generate
#' @param min optional minimum number
#' @param max optional maximum number
#' @param hole is a closed range of numbers not to include, e.g. c(1,2) would
#'   discard 1, 1.1 pi/2 and 2
#' @return vector length 5n+1 containing variety of difficult numbers for
#'   testing purposes
#' @importFrom stats runif
#' @export
random_test_numbers <- function(n = n_rnd,
                                min = NULL,
                                max = NULL,
                                hole = NULL) {
  x <-
    c(0,
      pi,
      sqrt(2),
      stats::runif(n),
      stats::runif(n, min = -1, max = 0),
      stats::runif(n, min = 0, max = 1),
      stats::runif(n, max = ifelse(is.null(max), .Machine$double.xmax, max)),
      stats::runif(n, min = ifelse(is.null(max), -.Machine$double.xmax, min)),
      stats::runif(n, min = -n * .Machine$double.xmin,
                   max =   n * .Machine$double.xmin)
    )
  #drop any generated numbers that didn't match the constraints
  if (!is.null(min)) x <- x[x >= min]
  if (!is.null(max)) x <- x[x <= max]
  #punch a hole in the range, if provided:
  if (!is.null(hole) && length(hole) == 2) {
    x <- x[!(x >= hole[1] & x <= hole[2])]
  }
  x
}

#' @rdname random_test_numbers
#' @export
random_test_integers <- function(n = n_rnd,
                                 min = -.Machine$integer.max,
                                 max = .Machine$integer.max,
                                 hole = NULL) {
  x <- suppressWarnings(
    unique(
      as.integer(
        random_test_numbers(n = n, min = min, max = max, hole = hole))))
  x[!is.na(x)]
}

#' @title generate random Dates or POSIXlt test datetimes
#' @description generate random \code{Date}s and \code{POSIXlt} test datetimes
#' @param n integer number to generate
#' @param origin Date defaults to Jan 1, 2000.
#' @param dayspread integer number of days either side of origin to pick random
#'   dates from, defaults to 150 years.
#' @return vector of POSIXlt datetimes or Dates
#' @export
random_test_dates <- function(n = n_rnd,
                              origin = as.Date("2000-01-01"),
                              dayspread = 365 * 150)
  as.Date(stats::runif(n, min = -dayspread, max = dayspread), origin)

#' @rdname random_test_dates
#' @export
random_test_posixlt_datetimes <- function(n = n_rnd,
                                          origin = as.Date("2000-01-01"),
                                          dayspread = 365 * 150) {
  as.POSIXlt(
    as.POSIXlt(random_test_dates(n, origin, dayspread)) +
      stats::runif(1, min = 0, max = 24 * 60 * 60)
  )
}

#' @rdname random_test_numbers
#' @param max_str_len integer scalar, maximum length of possible strings
#'   created, as distinct from number of strings given by \code{n}
#' @export
random_test_letters <- function(n = n_rnd, max_str_len = 257) {
  x <- c()
  for (i in 1:n) {
    x[length(x) + 1] <- paste(
      sample(
        c(LETTERS, letters),
        stats::runif(n = 1, min = 1, max = max_str_len),
        replace = TRUE),
      collapse = "")
  }
  x
}

#' @title extreme numbers
#' @description very biggest and smallest non-zero numbers the current machine
#'   can handle, positive and negative.
#' @keywords sysdata
#' @export
extreme_numbers <- c(
  .Machine$integer.max, -.Machine$integer.max,
  1L, -1L,
  .Machine$double.xmin,.Machine$double.xmax,
  -.Machine$double.xmin, -.Machine$double.xmax)

#' @title alternative \code{expect_that} from \code{testthat} which permutes all
#'   the inputs to a function which should give the same result where n args >=2
#'   and the function is commutative.
#' @description This makes a lot of assumptions, needs more testing. It can't
#'   handle mixed error/no error outcomes after permutation, which is an
#'   important feature to consider. The command following this function attaches
#'   this function to the testthat namespace. This means that it can call
#'   internal testthat functions, but does not mean it appears as
#'   testthat::expect_that_combine
#' @inheritParams testthat::expect_that
#' @examples
#'  expect_that_combine_all_args(sum(1, 2, 3),
#'   testthat::equals(6))
#' @return testthat result
#' @examples
#' \dontrun{
#' expect_that_combine_all_args(stop("a", "b"), testthat::throws_error())
#' expect_that_combine_all_args(sum(1, 2), testthat::equals(3))
#' expect_that_combine_first_arg(sum(c(1, 2)), testthat::equals(3))
#' }
#' @export
expect_that_combine_all_args <- function(object, condition,
                                         info = NULL, label = NULL) {
  cl <- substitute(object)
  # TODO: I now want to test for whether there are multiple top-level symbols
  func_name <- cl[[1]]
  args <- as.list(cl[-1])
  arg_names <- names(args)
  # can only handle flat lists of arguments when permuting
  stopifnot(identical(unlist(args, recursive = TRUE),
                      unlist(args, recursive = FALSE)))
  stopifnot(length(args) >= 2)
  arg_combs <- permute(unlist(args))
  arg_name_combs <- permute(arg_names)
  # now loop through all permutations
  for (comb in 1:dim(arg_combs)[1]) {
    arg_list <- as.list(arg_combs[comb, ])
    names(arg_list) <- arg_name_combs[comb, ]
    e <- testthat::expect_that(
      object    = do.call(as.character(func_name), arg_list),
      condition = condition,
      info      = paste0(
        info, "args = ",
        paste(arg_combs[comb, ], collapse = " ", sep = ","),
        sprintf(" (test iteration %d)", comb)
      ),
      label = label
    )
  }
  invisible(e)
}

#' @rdname expect_that_combine_all_args
#' @export
expect_that_combine_first_arg <- function(object, condition,
                                          info = NULL, label = NULL) {
  cl <- substitute(object)
  # TODO: see above
  func_name <- cl[[1]]
  args <- as.list(cl[-1])
  arg_one <- eval(args[[1]])  # c(1,2,3) has len 4 because not evaluated yet
  # can only handle flat lists of arguments when permuting? Does this apply when
  # working on first argument only?
  stopifnot(identical(unlist(args, recursive = TRUE),
                      unlist(args, recursive = FALSE)))
  stopifnot(length(arg_one) >= 2)

  # get the combinations of arguments
  arg_one_combs <- permute(arg_one)

  # now loop through all permutations
  for (comb in 1:dim(arg_one_combs)[1]) {
    e <- testthat::expect_that(
      object    = do.call(as.character(func_name),
                          c(list(arg_one_combs[comb, ]),
                            args[-1])),
      condition = condition,
      info      = paste0(
        info, "arg_one = ",
        paste(arg_one_combs[comb, ], collapse = " ", sep = ","),
        sprintf(" (test iteration %d)", comb)
      ),
      label     = label
    )
  }
  invisible(e)
}
