# Copyright (C) 2014 - 2019  Jack O. Wasey
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

#' convert factor or vector to numeric without warnings
#'
#' correctly converts factors to vectors, and then converts to
#'   numeric or integer, which may silently introduce NAs. Invisible rounding
#'   errors can be a problem going from numeric to integer, so consider adding
#'   tolerance to this conversion. \code{asIntegerNoWarn} silently
#'   \code{\link{floor}}s.
#'
#'   "are" functions return a value for each input, where is "allIs" functions
#'   return a single logical.
#' @param x is a vector, probably of numbers of characters
#' @return numeric vector, may have NA values
#' @aliases asIntegerNoWarn
#' @export
as_numeric_nowarn <- function(x) {
  if (is.factor(x)) x <- levels(x)[x]
  suppressWarnings(as.numeric(x))
}
asNumericNoWarn <- as_numeric_nowarn

#' @rdname as_numeric_nowarn
#' @export
as_integer_nowarn <- function(x)
  as.integer(asNumericNoWarn(x))
asIntegerNoWarn <- as_integer_nowarn

#' @rdname as_numeric_nowarn
#' @param tol tolerance when considering if two numbers are integers, default
#'   1e-9
#' @param na.ignore logical, if TRUE will pass through NA values, otherwise,
#'   they are marked FALSE.
#' @return logical vector
#' @examples
#' stopifnot(is_integerish("1"))
#' @export
is_integerish <- function(x, tol = 1e-9, na.ignore = FALSE) {
  if (is.null(x)) return(FALSE)
  stopifnot(is.numeric(tol), is.logical(na.ignore))
  stopifnot(length(tol) <= 1, length(na.ignore) <= 1)
  nas <- is.na(x)
  n <- asNumericNoWarn(x)
  i <- abs(n - round(n)) < tol
  i[is.na(i)] <- FALSE
  if (!na.ignore) {
    i[nas] <- FALSE
  } else {
    i[nas] <- NA_integer_
  }
  i
}

#' @describeIn as_numeric_nowarn Deprecated
#' @export
areIntegers <- is_integerish

#' Which elements of a character vector are numeric
#'
#' Takes a character vector and returns a logical vector of the same length,
#' indicating which values are numeric. `NA` is considered non-numeric. `NA` is
#' never returned from this function.
#' @param x character vector
#' @param extras character vector containing acceptable alternatives to numeric
#'   values which will result in returning \code{TRUE} for that element. Default
#'   is \code{c(".", "NA", NA)}.
#' @return logical vector of same length as input
#' @md
#' @examples
#' areNumeric(c("1", "2", "3"))
#' areNumeric(c("1L", "2.2"))
#' areNumeric(c("NA", NA, ".", "", "-1.9"))
#' @export
is_numeric_str <- function(x, extras = c(".", "NA", NA)) {
  if (is.null(x)) return(FALSE)
  old <- options(warn = -1)
  on.exit(options(old))
  x[x %in% c("", extras)] <- NA
  !is.na(as.numeric(x))
}

#' @describeIn is_numeric_str Deprecated
#' @export
areNumeric <- function(x, extras = c(".", "NA", NA)) {
  warning("Deprecated, use is_numeric_str")
  is_numeric_str(x, extras)
}

#' @title inverse of \%in\%
#' @description borrowed from Hmisc. See %in%. Original %in% is: match(x, table,
#'   nomatch = 0L) > 0L
#' @param x is the vector of values to be matched
#' @param table is actually a vector, to be matched against
#' @return logical vector of length of x
#' @export
"%nin%" <- function(x, table)
  match(x, table, nomatch = 0) == 0

#' @title count non-numeric elements
#' @description counts the number of non-numeric elements in a vector, without
#'   throwing warnings
#' @details did have \code{extras = c(".", "NA"))}
#' @param x is usually a charcter vector
#' @return integer
#' @export
countNotNumeric <- function(x)
  countIsNa(asNumericNoWarn(x))

#' @title count numeric elements
#' @description counts the number of numeric elements in a vector, without
#'   throwing warnings
#' @param x is usually a character vector
#' @return integer
#' @export
countNumeric <- function(x)
  length(x) - countNotNumeric(x)

#' @title count NA in vector
#' @description count the number of NAs in a vector. also consider `base::anyNA`
#' @param x vector
#' @return integer
#' @export
countIsNa <- function(x)
  sum(is.na(x))

#' @title Proportion of NA values in a vector
#' @description get fraction of NA in a vector
#' @param x is a vector which may have NA values
#' @return numeric proportion of NAs in the supplied vector
#' @export
propIsNa <- function(x)
  if (length(x)) countIsNa(x) / length(x) else 0L

#' @title running totals of number of non-NA values in consecutive fields
#' @description counts non-NA fields in first field, then progreses through
#'   fields, OR new field and saves running total for each field TODO: tests
#' @param d data.frame
#' @return vector of cumulative non-NA counts with names corresponding to the
#'   given data frame
#' @export
countNonNaCumulative <- function(d) {
  running <- rep(FALSE, dim(d)[1])
  apply(!is.na(d),
        MARGIN = 2,
        FUN = function(x, envir) {
          # update running total of non-NA count
          assign("running", running | x, envir = envir)
          sum(running)
        },
        environment()
  )
}

#' List all items in a package
#'
#' By default includes names beginning with '.'
#' @param package character scalar: name of the package
#' @param all.names = TRUE, set to FALSE to ignore items beginning with a period
#' @param pattern = optional pattern to match
#' @return character vector of package contents
#' @examples
#' lsp("jwutil")
#' tail(lsp("base"), 30L)
#' @export
lsp <- function(package, all.names = TRUE, pattern) {
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}

#' @title convert separate lists of dates and times to POSIXlt objects
#' @description Some datetime data is presented as a separate dates and times.
#'   This function restores the full date-time.
#' @param dts vector of dates, in string format \%Y-\%m-\%d or simple R Date
#'   objects
#' @param tms vector of times, i.e. number in range 0 to 2400, as string or
#'   integer, with or without trailing zeros
#' @template verbose
#' @return vector of POSIXlt date-times
#' @export
add_time_to_date <- function(tms, dts, verbose = FALSE) {
  if (length(dts) != length(tms)) {
    stop("must have matching lengths of date and time vectors.
         I got: %d and %d", length(dts), length(tms))
  }
  if (class(dts) %nin% c("Date", "character") && !is.na(dts)) {
    stop(paste(
      "date must be of class Date, character, but received: %s",
      class(dts)
    ))
  }
  # if a time part is given in the date field, this is an error
  if (is.character(dts) && any(grepl(pattern = "\\S\\s\\S", dts))) {
    stop(
      "suspect time is already given with date argument, \
         which invalidates this entire function. e.g. %s",
      dts[grepl(pattern = "\\S\\s\\S", dts)][1]
    )
  }
  # convert to Date (may already be Date, but that's fine) any conversion error
  # in any item will result in an error. an alternative strategy would be to
  # individually tryCatch converting each Date, returning warnings, NA, or
  # detailed error message. TODO
  dts <- as.Date(dts)
  # a single NA value could appear as type logical
  if (class(tms) %nin% c("numeric", "integer", "character") && !is.na(tms)) {
    stop(
      "time must be numeric or character, but got class for times of '%s'.",
      class(tms)
    )
  }
  # this is a data error, not a programming error, stop
  if (any(dts < as.Date("1850-01-01"), na.rm = TRUE)) {
    stop("some dates are before 1850: ", dts[dts < as.Date("1850-01-01")])
  }
  # could alternatively set NA, warn and continue.
  # Let NA be valid:
  if (!all(isValidTime(tms, na.rm = TRUE))) {
    warning(sprintf(
      "%d invalid time(s) received, replacing with NA",
      sum(isValidTime(tms, na.rm = TRUE))
    ))
    tms[!isValidTime(tms)] <- NA
  }
  if (is.character(tms)) tms <- gsub(":", "", tms, fixed = TRUE)
  if (verbose) {
    message(paste("working with times:", tms,
                  collapse = ", ", sep = ", "
    ), capture = TRUE)
  }
  # convert to integer, then back to string later. THis is horrible.
  tms <- asIntegerNoWarn(tms)
  bad_range <- any(tms < 0 || tms > 2359)
  if (!is.na(bad_range) && bad_range) {
    warning("invalid times found. Setting to NA:", tms, capture = TRUE)
    tms[bad_range] <- NA
  }
  timesfourzeros <- formatC(tms, width = 4, format = "d", flag = "0")
  strptime(paste(dts, timesfourzeros, sep = " "), "%Y-%m-%d %H%M")
}

#' @title check if a time is valid in 24h clock
#' @description allow leading and trailing space, optional colon in middle, 2400
#'   is not allowed. TODO: can lubridate do this better?
#' @param tms is a vector of characters which may represent times
#' @param na.rm logical if true, will ignore NA values, otherwise these will
#'   test as invalid.
#' @return logical vector, with NA out if NA given
#' @export
isValidTime <- function(tms, na.rm = FALSE) {
  if (na.rm) tms <- tms[!is.na(tms)]
  grepl("^[[:space:]]*([01]?[0-9]|2[0-3])?:?[0-5]?[0-9][[:space:]]*$", tms)
}

#' Shuffle a vector
#'
#' Randomly shuffle the order of a vector or list. This is to improve quality of
#' bad data to throw at functions when testing.
#' @param x list or vector
#' @return list or vector of same length as input, (probably) in a different
#'   order
#' @examples
#' set.seed(1441)
#' shuffle(LETTERS)
#' @export
shuffle <- function(x)
  sample(x = x, size = length(x), replace = FALSE, prob = NULL)

#' Generate all permutations of input, reusing values in each result row
#'
#' Expand the given vector into all possible values in each location, with or
#' without duplicates.
#' @param x list or vector
#' @param unique logical, if `TRUE`, the default, only unique results are
#'   returned
#' @return data frame, each row being one permutation
#' @md
#' @examples
#' ltr <- c("a", "b", "c")
#' x <- permuteWithRepeats(ltr, unique = FALSE)
#' print(x)
#' stopifnot(nrow(x) == length(ltr)^length(ltr))
#' # duplicate results are dropped
#' y <- permuteWithRepeats(c("X", "Y", "Y"))
#' print(y)
#' stopifnot(nrow(y) == 2^3)
#' z <- permuteWithRepeats(c("X", "Y", "Y", "Y"))
#' stopifnot(nrow(z) == 2^4)
#' a <- permuteWithRepeats(c(1, 2, 3, 1))
#' stopifnot(nrow(a) == 3^4)
#' @export
permuteWithRepeats <- function(x, unique = TRUE) {
  stopifnot(length(x) < 8)
  y <- expand.grid(rep(list(unlist(x)), times = length(unlist(x))))
  if (unique) {
    y <- unique(y)
    rownames(y) <- NULL
  }
  y
}

#' Generate all permutations of input
#'
#' Systematically permute the input vector or list, which is very slow for long
#' x. Am amazed something this simple isn't either in base R, or in a
#' straightforward form in a package.
#'
#' TODO: limit to a certain cut-off, after which we randomly sample
#' @param x list or vector
#' @return data frame, each row being one permutation
#' @examples
#' ltr <- c("a", "b", "c", "d")
#' x <- permute(ltr)
#' print(x)
#' stopifnot(nrow(x) == factorial(length(ltr)))
#' ltr <- c("a", "b", "b")
#' x <- permute(ltr)
#' print(x)
#' stopifnot(nrow(x) == factorial(length(ltr)))
#' @export
permute <- function(x) {
  if (is.null(x)) return()
  stopifnot(length(x) < 13) # factorial size, so limit for sanity
  # break out of recursion:
  if (length(x) == 2) return(rbind(x, c(x[2], x[1])))
  res <- c()
  # take each one and place it first, then recurse the rest:
  for (element in 1:length(x)) {
    sub_combs <- Recall(x[-element]) # recurse
    new_combs <- cbind(x[element], sub_combs)
    res <- rbind(res, new_combs)
  }
  unname(res)
}

#' @title all unique combinations of a vector and all its non-zero subsets
#' @description all unique combinations of a vector and all its non-zero subsets
#' @param x vector to be subsetted and combined
#' @importFrom  utils combn
#' @return list of vectors with all combinations of x and its subsets
#' @examples
#' combn_subset(c("a", "b"))
#' combn_subset(c(10, 20, 30))
#' combn_subset(NULL)
#' @export
combn_subset <- function(x) {
  res <- list()
  for (n in seq_along(x)) {
    r <- utils::combn(x, n, simplify = FALSE)
    r2 <- lapply(t(r), FUN = function(y) unlist(y))
    res <- c(res, r2)
  }
  unique(res)
}

#' selects columns from a data frame using an optimization function
#'
#' The optimization function is called with the data frame `x` and the names of
#' each combination of the names of `x`'s columns. An example of real-world
#' usage is to automate selection of columns according to the optimization
#' function.
#' @param x data frame
#' @param fun function which takes parameters `x = data.frame, n = columns`
#' @template verbose
#' @md
#' @examples
#' j <- data.frame(a = 1:5, b = 6:2, c = c(0, 2, 4, 6, 8))
#' opt_binary_brute(j)
#' j[1, 1] <- NA
#' j[1:4, 2] <- NA
#' my_opt_fun <- function(x, n) sum(!unlist(lapply(x, is.na)))
#' opt_binary_brute(j, fun = my_opt_fun)
#' @export
opt_binary_brute <- function(x, fun = opt_binary_fun, verbose = FALSE) {
  n <- names(x)
  all_cmbs <- combn_subset(n)
  best_min <- 1e9
  best_min_by_len <- c(rep(best_min, times = length(n) - 1))
  best_cmb_by_len <- as.list(rep("", times = length(n) - 1))
  for (cmb in rev(all_cmbs)[-1]) {
    len_cmb <- length(cmb)
    optim <- fun(x, cmb)
    if (optim < best_min_by_len[len_cmb]) {
      if (verbose) {
        message(
          "best combination for length: ", len_cmb, " is ",
          paste(cmb, collapse = ", "), ", optim = ", optim
        )
      }
      best_min_by_len[len_cmb] <- optim
      best_cmb_by_len[[len_cmb]] <- cmb
    }
  }
  list(
    best_min_by_len = best_min_by_len,
    best_cmb_by_len = best_cmb_by_len
  )
}

# stupid example optimization metric function
opt_binary_fun <- function(x, n) {
  sum(colSums(x[n])) / length(n)
}

#' Are we running on Linux, Mac or Windows?
#'
#' @return logical
#' @export
platformIsLinux <- function()
  Sys.info()[["sysname"]] == "Linux"

#' @rdname platformIsLinux
#' @export
platformIsWindows <- function()
  Sys.info()[["sysname"]] == "Windows"

#' @rdname platformIsLinux
#' @export
platformIsMac <- function()
  Sys.info()[["sysname"]] == "Darwin"

#' @title read \code{.xlsx} file, interpret as CSV, and return a data frame
#' @description currently relies on Linux xlsx2csv command, but could
#'   potentially be done with VB script in Windows. This offers a different
#'   backend to other Excel parsing functions in R,
#' @param file is the path to the \code{.xlsx} file
#' @return data frame
#' @seealso \code{readxl} package by Hadley Wickham
#' @importFrom utils read.delim
#' @export
read_xlsx_linux <- function(file) {
  if (platformIsWindows()) {
    stop("can only convert XLSX on linux using xlsx2csv command")
  }
  csvfile <- tempfile()
  on.exit(unlink(csvfile), add = TRUE)
  system(paste0(
    "xlsx2csv --delimiter=tab --dateformat=%m-%d-%y \"",
    file, "\" > ", csvfile
  ))
  utils::read.delim(csvfile)
}

#' @title build simple linear formula from variable names
#' @description build simple linear formula from variable names given by two
#'   character vectors. TODO: allow unquoted names.
#' @param left character vector
#' @param right character vector
#' @return formula
#' @importFrom stats as.formula
#' @examples
#' print(f <- build_formula(left = "A", right = c("B", "C")))
#' class(f)
#' build_formula(left = "Species", right = names(iris)[1:4])
#' @export
build_formula <- function(left, right) {
  stopifnot(is.character(left) && length(left) == 1)
  stopifnot(is.character(right) && length(right) > 0)
  stats::as.formula(
    paste(
      paste(left, collapse = "+"),
      paste(right, collapse = "+"),
      sep = "~"
    )
  )
}

#' @rdname build_formula
#' @export
buildLinearFormula <- build_formula

#' @title inverse which
#' @description for a given vector of ordinals which would reference items in a
#'   vector, list etc, \code{invwhich} returns a logical vector with TRUE for
#'   the cited positions. If length is not provided, the maximum index is used.
#' @param which integer vector of indices, as would be produced by \code{which}
#' @param len integer scalar: length of return vector, defaults to
#'   \code{max(which)}
#' @return logical vector of length \code{length}
#' @export
invwhich <- function(which, len = max(which)) {
  stopifnot(all(which > 0))
  stopifnot(all(is_integerish(which)))
  stopifnot(length(len) > 0)
  stopifnot(identical(areIntegers(len), TRUE))
  is.element(seq_len(len), which)
}

#' @title recursive remove
#' @description search through environments until the variables in the list
#'   \code{x} are all gone. This doesn't delete functions.
#' @param x variables to annihilate
#' @param envir environment to start at, defaults to calling frame.
#' @export
rm_r <- function(x, envir = parent.frame()) {
  suppressWarnings({
    while (any(vapply(x, exists, logical(1),
                      mode = "numeric", inherits = TRUE, envir = envir
    )))
      rm(list = x, envir = envir, inherits = TRUE)
  })
}

#' Summarize objects
#'
#' Get type, size (bytes) and dimensions of objects
#' @param env Environment to search, default is the parent frame
#' @param pattern regex pattern to match objects of interest
#' @param order.by which column to order by
#' @param decreasing default is \code{TRUE}
#' @param head default is \code{FALSE} but if true, just show top \code{n}
#' @param n number to show if limiting to \code{head}
#' @importFrom utils object.size
#' @md
#' @export
ls.objects <- function(env = parent.frame(), pattern, order.by,
                       decreasing = FALSE, head = FALSE, n = 5) {
  nms <- ls(envir = env, pattern = pattern)
  if (!length(names)) {
    message("No objects found.")
    return()
  }
  obj.class <- vapply(mget(nms, envir = env), class, character(1))
  obj.mode <- vapply(mget(nms, envir = env), mode, character(1))
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- vapply(mget(nms, envir = env), utils::object.size, double(1))
  obj.rows <- rep(NA_integer_, length(nms))
  obj.cols <- rep(NA_integer_, length(nms))
  for (n in seq_along(nms)) {
    w <- get(nms[n], envir = env)
    d <- dim(w)
    if (!is.null(d) && !is.na(d)) {
      obj.rows[n] <- d[1]
      obj.cols[n] <- d[2]
    } else if (is.vector(w)) {
      obj.rows[n] <- length(w)
      obj.cols[n] <- NA
    }
    else {
      obj.rows[n] <- NA
      obj.cols[n] <- NA
    }
  }
  out <- data.frame(obj.type, obj.size, obj.rows, obj.cols)
  names(out) <- c("Type", "Size", "Len/Rows", "Columns")
  if (!missing(order.by)) {
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  }
  if (head) {
    out <- head(out, n)
  }
  out
}

#' @title show largest objects
#' @description https://gist.github.com/1187166.git Taken from
#'   http://stackoverflow.com/questions/1358003/\
#'   tricks-to-manage-the-available-memory-in-an-r-session
#' @param ... arguments passed on to \code{.ls.objects}
#' @param n scalar integer, number of objects to show
#' @export
lsos <- function(..., n = 10)
  ls.objects(
    env = parent.frame(), ...,
    order.by = "Size", decreasing = TRUE, head = TRUE, n = n
  )

#' @title is the object a \code{Date}
#' @description copied from lubridate
#' @param x object to test
#' @return logical
#' @export
is.Date <- function(x)
  methods::is(x, "Date")

#' Extract code from knitr vignette and source it
#'
#' Extract code from knitr vignette and `source` it.
#' @param input path to file as single character string
#' @param documentation single integer value passed on to `knitr::purl`. An
#'   integer specifying the level of documentation to go the tangled script: 0
#'   means pure code (discard all text chunks); 1 (default) means add the chunk
#'   headers to code; 2 means add all text chunks to code as roxygen comments
#' @param ... further parameters passed to \code{source}
#' @md
#' @export
source_purl <- function(input, documentation = 1L, ...) {
  output <- tempfile()
  on.exit(unlink(output))
  stopifnot(is.integer(documentation) && length(documentation) == 1L)
  knitr::purl(input, output, quiet = TRUE, documentation = documentation)
  source(output, ...)
}

#' Find minimum R version required for package
#'
#' Recursively search dependencies for R version, and find the highest stated R
#' version requirement.
#' @source Based on ideas from
#'   http://stackoverflow.com/questions/38686427/determine-minimum-r-version-for-all-package-dependencies
#' @param pkg string with name of package to check
#' @examples
#' base <- c(
#'   "base", "compiler", "datasets", "grDevices", "graphics",
#'   "grid", "methods", "parallel", "profile", "splines", "stats",
#'   "stats4", "tcltk", "tools", "translations"
#' )
#' \dontrun{
#' base_reqs <- lapply(base, min_r_version)
#' contrib <- c(
#'   "KernSmooth", "MASS", "Matrix", "boot",
#'   "class", "cluster", "codetools", "foreign", "lattice",
#'   "mgcv", "nlme", "nnet", "rpart", "spatial", "survival"
#' )
#' contrib_reqs <- lapply(contrib, min_r_version)
#' min_r_version("icd")
#' }
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages contrib.url compareVersion
#' @export
min_r_version <- function(pkg) {
  avail <- utils::available.packages(
    utils::contrib.url("https://cloud.r-project.org")
  )
  deps <- tools::package_dependencies(pkg, db = avail, recursive = TRUE)
  if (is.null(deps)) stop("package not found")
  pkgs <- deps[[1]]
  repo <- getOption("repo")
  if (is.null(repo)) repo <- "https://cloud.r-project.org"
  matches <- avail[, "Package"] %in% pkgs
  pkg_list <- avail[matches, "Depends"]
  vers <- grep("^R$|^R \\(.*\\)$", pkg_list, value = TRUE)
  vers <- gsub("[^0-9.]", "", vers)
  if (length(vers) == 0) return("Not specified")
  max_ver <- vers[1]
  if (length(vers) == 1) return(max_ver)
  for (v in 2:length(vers))
    if (utils::compareVersion(vers[v], max_ver) > 0) max_ver <- vers[v]
  max_ver
}

#' Load packages with `library`, installing any which are missing
#' @param pkgs character vector of packages to load and attach, with
#'   installation if necessary
#' @importFrom utils install.packages
#' @md
#' @export
reqinst <- function(pkgs) {
  for (pkg in pkgs) {
    if (suppressPackageStartupMessages(
      !require(pkg,
               character.only = TRUE,
               quietly = TRUE,
               warn.conflicts = FALSE
      )
    )) {
      utils::install.packages(pkg, quiet = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

#' Get the first/only argument of the last run command
#'
#' This is experimental and writes to the calling frame, which, if in
#' interactive mode, is probably the global environment. We write to a horrible
#' and unusual (although well-known) variable name, which will avoid name
#' conflicts.
#' @return Returns the evaluated value of the first argument to first argument
#'   of the previous R command, if it was a function call with parentheses.
#'   Primarily, the function is invoked for its side effect of adding of
#'   modifying `!$` in the global environment.
#' @keywords internal
#' @references
#' \url{http://kevinushey.github.io/blog/2015/02/02/rprofile-essentials/}
#' @md
bang_dollar <- function() {
  stopifnot(interactive())
  hf <- tempfile()
  utils::savehistory(hf)
  h <- readLines(hf)
  unlink(hf)
  last_cmd <- h[[length(h) - 1]]
  end_cmd <- sub(".*\\(", "", last_cmd)
  first_arg <- sub("[,)].*", "", end_cmd)
  res <- eval(first_arg, envir = parent.frame())
  assign(".bangdollar", res, envir = parent.frame())
  invisible(res)
}

`!$` <- function() {
  stopifnot(interactive())
  hf <- tempfile()
  utils::savehistory(hf)
  h <- readLines(hf)
  unlink(hf)
  last_cmd <- h[[length(h) - 1]]
  end_cmd <- sub(".*\\(", "", last_cmd)
  first_arg <- sub("[,)].*", "", end_cmd)
  if (length(first_arg) == 0) return()
  res <- eval(first_arg, envir = parent.frame())
  res
}

#' Take clipboard contents, and write sorted character vector back
#' @param cl Name of class to give to data before sorting, default is
#'   \code{NULL}.
#' @export
sort_clip_char <- function(cl = NULL) {
  tf <- tempfile()
  on.exit(unlink(tf))
  unsorted <- eval(
    parse(text = paste(clipr::read_clip(), collapse = "")
    )
  )
  if (!is.null(cl)) class(x) <- cl
  x <- sort(unsorted)
  dput(file = tf, x)
  clipr::write_clip(paste(readLines(tf)))
}
