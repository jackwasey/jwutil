#' @title test coverage
#' @description This function searches for all functions in a package, traces
#'   them all (just to see function entry, not all code paths), and parses the
#'   trace output from running all the testthat tests. I don't think
#'   finer-grained analysis of code paths within functions is possible with this
#'   mechanism, although it is possible to trace individual lines of the parsed
#'   source code, I don't think it could trace within an \code{if} statement
#'   contained on a single line. This is a lot better than nothing.
#'   The testing relies on the tests/testthat directory to exist. It doesn't call
#'   test() directly, because this results in tracing the wrong functions. Maybe
#'   Hadley Wickham could incorporate this into testthat.
#' @inheritParams lsf
#' @export
testCoverage <- function(pkg = getPackageName(parent.frame()), verbose = FALSE) {
  require(testthat)
  require(devtools)
  if (verbose) message("pkg = ", pkg)
  pkgenvir = as.environment(paste0("package:", pkg))
  funs <- lsf(pkg) # see function in util which lists contents of a package
  if (verbose)
    message(sprintf("functions found in %s are: %s", pkg, paste(funs, collapse=", ")))

  tfcon <-file(tempfile(), open='w+')
  sink(file = tfcon, type = "message")
  for (f in funs) {
    trace(f, where = pkgenvir, print = TRUE)
  }
  sink()
  #close(tfcon)

  trace_output <- capture.output(test_dir("tests/testthat/", reporter=SilentReporter()))
  #trace_output <- capture.output(test_file("tests/test-all.R", reporter=SilentReporter()))

  tfcon <-file(tempfile(), open='w+')
  sink(file = tfcon, type = "message")
  for (f in funs) {
    capture.output(untrace(f, where = pkgenvir))
  }
  sink()
  #close(tfcon)

  everytestedfun <- unique(
    lapply(trace_output,
           function(x) unlist(
             regmatches(
               x = x,
               m = regexec(
                 pattern = "^.* ([[:graph:]]*?)\\(.*",
                 text = x)
             )
           )[-1]
    )
  )
  tested   <- sort(funs[ funs %in% everytestedfun])
  untested <- sort(funs[!funs %in% everytestedfun])
  coverage <- length(tested)/length(funs)

  if (verbose) {
    message("tested functions are: ", paste(tested, collapse = ", "))
    message("untested functions are: ", paste(untested, collapse = ", "))
    message(sprintf("test coverage is: %.2g percent", coverage*100))
  }

  list(untested = untested, tested = tested, coverage = coverage)
}

#' @title list all functions in a package
#' @param pkg character string containing package name
#' @return character vector of functions in given package
#' @export
lsf <- function(pkg) {

  envirName <- paste("package", pkg, sep = ":")
  everything <- ls(pos = envirName, all.names = TRUE)

  funcs <- c()
  for (e in everything) {
    if (is.function(get(e, envir = as.environment(envirName), inherits = F)))
      funcs <- append(funcs, e)
  }
  funcs
}


#' @title shuffle
#' @description shuffle the order of a vector or list. This is to improve
#'   quality of bad data to throw at functions when testing.
#' @param x list or vector
#' @return list or vector of same length as input, (probably) in a different
#'   order
#' @export
shuffle <- function(x)
  sample(x, length(x), replace = FALSE, prob = NULL)

numbers_to_long_and_float <- function(..., na.rm = TRUE) {
  #browser()
  x <- flatten_list(list(...))
  # drop any NA values. Very big numbers not representable by 32 bit integers,
  # give NA with warning. For test case generation, usually we will want to
  # remove NAs.
  suppressWarnings(
    x <- flatten_list(list(as.integer(x)), list(as.double(x)), na.rm = na.rm)
  )
  x
}

zeroes <- list(0L, 0.0) # long integer and double float

bad_input <- c(
  list(c(), NA, list()),
  c(),
  list(list(1,2),jack="test", c(1,2,3), data.frame()),
  NULL,
  NA,
  this="jack",
  data.frame(j=1:10,k=11:20),
  matrix(nrow=2,ncol=2),
  as.POSIXct(Sys.time(), "GMT"),
  .leap.seconds
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
#' @export
random_test_numbers <- function(n = 100,
                                min = NULL,
                                max = NULL,
                                hole = NULL) {
  x <- c(0,
         pi,
         sqrt(2),
         runif(n),
         runif(n, min = -1, max = 0),
         runif(n, max = .Machine$double.xmax),
         runif(n, min = -.Machine$double.xmax),
         runif(n, min = -n * .Machine$double.xmin, max = n * .Machine$double.xmin)
  )
  #drop any generated numbers that didn't match the constraints
  if (!is.null(min)) { x <- x[x >= min] }
  if (!is.null(max)) { x <- x[x <= max] }

  #punch a hole in the range, if provided:
  if (!is.null(hole) && length(hole) == 2) {
    x <- x[!(x >= hole[1] & x <= hole[2])]
  }
  x
}

#' @rdname random_test_numbers
#' @param origin Date defaults to Jan 1, 2000.
#' @param dayspread integer number of days either side of origin to pick random dates from
#' @return vector of Dates
#' @export
random_test_dates <- function(n = 100,
                              origin=as.Date("2000-01-01"),
                              dayspread = 365*150) {
  as.Date(runif(n, min = -dayspread, max = dayspread), origin)
}

#' @rdname random_test_numbers
#' @param origin Date defaults to Jan 1, 2000.
#' @param dayspread integer number of days either side of origin to pick random dates from
#' @return vector of Dates
#' @export
random_test_posixlt_datetimes <- function(n = 100,
                                          origin = as.Date("2000-01-01"),
                                          dayspread = 365*150) {
  as.POSIXlt(as.POSIXlt(random_test_dates(n, origin, dayspread)) + runif(1, min = 0, max=24*60*60))
}

#' @rdname random_test_numbers
#' @export
random_test_letters <- function(n = 100, maxlen = 257) {
  paste(sample(c(LETTERS,letters, 0:9), runif(n, min = 0, max = maxlen), replace = TRUE), collapse = "")
}

extreme_numbers <- c(
  .Machine$integer.max,
  -.Machine$integer.max,
  .Machine$double.xmin,
  .Machine$double.xmax,
  -.Machine$double.xmin,
  -.Machine$double.xmax)
