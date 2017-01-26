# Copyright (C) 2014 - 2017  Jack O. Wasey
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

#' check whether character vector represents all numeric values
#'
#' check whether all the items of input vector are numeric without throwing
#' warning derived from Hmsic package
#' @param x is a character vector to be tested
#' @param extras is a vector of character strings which are counted as NA
#'   values, defaults to '.' and 'NA'. Also allow \code{NA}.
#' @return logical scalar
#' @export
allIsNumeric <- function(x, extras = c(".", "NA", NA)) {
  xs <- x[x %nin% c("", extras)]
  suppressWarnings(!any(is.na(as.numeric(xs))))
}

#' check whether vector represents all integer values, not that the same as
#' \code{is.integer}
#'
#' check whether all the items of input vector are integer as.integer
#' @param x is a vector to be tested
#' @param tol single numeric, default if less than 1e-9 from an integer then
#'   considered an integer.
#' @param na.rm single logical, passed on to \code{all}
#' @return logical scalar
#' @export
allIsInteger <- function(x, tol =  1e-9, na.rm = TRUE)
  all(  # don't count NA as false automatically
    areIntegers(x, tol = tol, na.ignore = TRUE),
    na.rm = na.rm
  )

#' convert to character vector without warning
#' @param x vector, typically numeric or a factor
#' @return character vector
#' @export
as_char_no_warn <- function(x) {
  if (is.character(x))
    return(x)
  old <- options(warn = -1)
  on.exit(options(old))
  if (is.integer(x))
    fastIntToStringRcpp(x)
  if (is.factor(x))
    levels(x)[x]
  else
    as.character(x)
}

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
asNumericNoWarn <- function(x) {
  if (is.factor(x)) x <- levels(x)[x]
  suppressWarnings(as.numeric(x))
}

#' @rdname asNumericNoWarn
#' @export
asIntegerNoWarn <- function(x)
  as.integer(asNumericNoWarn(x))


#' @rdname asNumericNoWarn
#' @param tol tolerance when considering if two numbers are integers, default
#'   1e-9
#' @param na.ignore logical, if TRUE will pass through NA values, otherwise,
#'   they are marked FALSE.
#' @return logical vector
#' @export
areIntegers <- function(x, tol = 1e-9, na.ignore = FALSE) {
  if (is.null(x)) return(FALSE)
  stopifnot(is.numeric(tol), is.logical(na.ignore))
  stopifnot(length(tol) <= 1, length(na.ignore) <= 1)
  nas <- is.na(x)
  n <- asNumericNoWarn(x)
  i <- abs(n - round(n)) < tol
  i[is.na(i)] <- FALSE
  if (!na.ignore)
    i[nas] <- FALSE
  else
    i[nas] <- NA_integer_
  i
}

#' @title which elements of a vector are numeric
#' @description test without throwing a warning
#' @param x vector
#' @param extras character vector containing acceptable alternatives to numeric
#'   values which will result in returning \code{TRUE} for that element. Default
#'   is \code{c(".", "NA", NA)}.
#' @return logical vector of same length as input
#' @examples
#' areNumeric(c("1","2","3"))
#' areNumeric(c("1L", "2.2"))
#' areNumeric(c("NA", NA, ".", "", "-1.9"))
#' @export
areNumeric <- function(x, extras = c(".", "NA", NA)) {
  if (is.null(x)) return(FALSE)
  old <- options(warn = -1)
  on.exit(options(old))
  x[x %in% c("", extras)] <- NA
  !is.na(as.numeric(x))
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

#' @title read file from zip at URL
#' @description downloads zip file, and opens named file \code{filename}, or the
#'   single file in zip if \code{filename} is not specified. FUN is a function,
#'   with additional arguments to FUN given by \dots.
#'   @details TODO: update from \code{icd} package
#' @param url character vector of length one containing URL of zip file.
#' @param filename character vector of length one containing name of file to
#'   extract from zip. If not specified, and the zip contains a single file,
#'   then this single file will be used.
#' @param FUN function used to process the file in the zip, defaults to
#'   readLines. The first argument to FUN will be the path of the extracted
#'   \code{filename}
#' @param \dots further arguments to FUN
#' @export
read.zip.url <- function(url, filename = NULL, FUN = readLines, ...) {
  stopifnot(length(filename) <= 1)
  stopifnot(is.character(url), length(url) == 1)
  zipfile <- tempfile()
  on.exit(unlink(zipfile), add = TRUE)
  utils::download.file(url = url, destfile = zipfile, quiet = TRUE)
  zipdir <- tempfile()
  on.exit(unlink(zipfile, recursive = TRUE), add = TRUE)
  dir.create(zipdir)
  utils::unzip(zipfile, exdir = zipdir)  # files="" so extract all
  files <- list.files(zipdir, recursive = TRUE)
  if (is.null(filename)) {
    if (length(files) == 1) {
      filename <- files
    } else {
      stop("multiple files in zip, but no filename specified: ",
           paste(files, collapse = ", "))
    }
  } else
    stopifnot(filename %in% files)

  do.call(FUN, args = c(list(file.path(zipdir, filename), warn = FALSE),
                        list(...)))
}

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
#' @description count the number of NAs in a vector
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
  if (length(x)) {
    countIsNa(x) / length(x)
  } else {
    0
  }

#' @title count which combinations of fields have at least one non-NA
#' @description cycles through the given data frame twice, and applies logical
#'   OR to all elements of each column it then counts how many of these pairs
#'   are not-na, i.e. have at least one non-NA value TODO: tests
#' @param d data.frame
#' @return matrix with nrow and ncol being the number of fields in the given
#'   dataframe
#' @export
countNonNaPairs <- function(d) {
  stop("needs thinking through")
  apply(!is.na(d),
        MARGIN = 2,
        FUN = function(y) {
          apply(is.na(d),
                MARGIN = 2,
                FUN = function(x, y) sum(x | y),
                y)
        }
  )
}

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
          #update running total of non-NA count
          assign("running", running | x, envir = envir)
          sum(running)
        },
        environment()
  )
}

#' @title list all items in a package
#' @description default to including (?private) functions beginning with '.'
#' @param package is the (unquoted) name of the package
#' @param all.names = TRUE, set to FALSE to ignore items beginning with a period
#' @param pattern = optional pattern to match
#' @return character vector of package contents
#' @export
lsp <- function(package, all.names = TRUE, pattern) {
  package <- deparse(substitute(package))
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

  if (length(dts) != length(tms))
    stop("must have matching lengths of date and time vectors.
         I got: %d and %d", length(dts), length(tms))

  if (class(dts) %nin% c("Date", "character") && !is.na(dts))
    stop(paste("date must be of class Date, character, but received: %s",
               class(dts)))

  # if a time part is given in the date field, this is an error
  if (is.character(dts) && any(grepl(pattern = "\\S\\s\\S", dts)))
    stop("suspect time is already given with date argument, \
         which invalidates this entire function. e.g. %s",
         dts[grepl(pattern = "\\S\\s\\S", dts)][1])

  # convert to Date (may already be Date, but that's fine) any conversion error
  # in any item will result in an error. an alternative strategy would be to
  # individually tryCatch converting each Date, returning warnings, NA, or
  # detailed error message. TODO
  dts <- as.Date(dts)

  # a single NA value could appear as type logical
  if (class(tms) %nin% c("numeric", "integer", "character") && !is.na(tms))
    stop("time must be numeric or character, but got class for times of '%s'.",
         class(tms))

  # this is a data error, not a programming error, stop
  if (any(dts < as.Date("1850-01-01"), na.rm = TRUE)) {
    stop("some dates are before 1850: ", dts[dts < as.Date("1850-01-01")])
    # could alternatively set NA, warn and continue:
  }

  # let NA be valid:
  if (!all(isValidTime(tms, na.rm = TRUE))) {
    warning(sprintf("%d invalid time(s) received, replacing with NA",
                    sum(isValidTime(tms, na.rm = TRUE))))
    tms[!isValidTime(tms)] <- NA
  }

  # drop colons, if any
  if (is.character(tms))  tms <- gsub(":", "", tms, fixed = TRUE)

  if (verbose) message(paste("working with times:", tms,
                             collapse = ", ", sep = ", "),
                       capture = TRUE)

  # convert to integer, then back to string later. THis is horrible.
  tms <- asIntegerNoWarn(tms)
  bad_range <- any(tms < 0 || tms > 2359)
  if (!is.na(bad_range) && bad_range)  {
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

#' @title shuffle
#' @description randomly shuffle the order of a vector or list. This is to
#'   improve quality of bad data to throw at functions when testing.
#' @param x list or vector
#' @return list or vector of same length as input, (probably) in a different
#'   order
#' @export
shuffle <- function(x)
  sample(x = x, size = length(x), replace = FALSE, prob = NULL)

#' @title generate all permutations of input, reusing values in each result row
#' @description systematically permute the input vector or list
#' @param x list or vector
#' @return data frame, each row being one permutation
#' @export
permuteWithRepeats <- function(x) {
  stopifnot(length(x) < 8)
  expand.grid(rep(list(unlist(x)), times = length(unlist(x))))
}

#' @title generate all permutations of input
#' @description systematically permute the input vector or list, which is very
#'   slow for long x. Am amazed something this simple isn't either in base R, or
#'   in a straightforward form in a package.
#'
#'   TODO: limit to a certain cut-off, after which we randomly sample
#' @param x list or vector
#' @return data frame, each row being one permutation
#' @export
permute <- function(x) {
  if (is.null(x)) return()

  stopifnot(length(x) < 13)  # factorial size, so limit for sanity
  # break out of recursion:
  if (length(x) == 2) return(rbind(x, c(x[2], x[1])))

  res <- c()

  #take each one and place it first, then recurse the rest:
  for (element in 1:length(x)) {
    sub_combs <- Recall(x[ -element])  # recurse
    new_combs <- cbind(x[element], sub_combs)
    res <- rbind(res, new_combs)
  }
  unname(res)
}

#' @title all unique combinations of a vector and all its non-zero subsets
#' @description all unique combinations of a vector and all its non-zero subsets
#' @param x vector to be subsetted and combined
#' @return list of vectors with all combinations of x and its subsets
#' @export
combn_subset <- function(x) {
  res <- list()
  for (n in 1:length(x)) {
    r <- utils::combn(x, n, simplify = FALSE)
    r2 <- lapply(t(r), FUN = function(y) unlist(y))
    res <- c(res, r2)
  }
  unique(res)
}

#' @title optimizes a function for all combinations of all subsets
#' @description takes a data frame and optimization function
#' @param x data frame
#' @param fun function which takes parameters x = data.frame, n = columns
#' @template verbose
#' @export
opt_binary_brute <- function(x, fun = opt_binary_fun, verbose = TRUE) {

  n <- names(x)
  all_cmbs <- combn_subset(n)
  best_min <- 1e9
  best_min_by_len <- c(rep(best_min, times = length(n)))
  best_cmb_by_len <- as.list(rep("", times = length(n)))
  for (cmb in rev(all_cmbs)) {
    len_cmb <- length(cmb)
    optim <- fun(x, cmb)
    if (optim < best_min_by_len[len_cmb]) {
      if (verbose) message("best combination for length: ", len_cmb, " is ",
                           paste(cmb, collapse = ", "), " and optim: ", optim)
      best_min_by_len[len_cmb] <- optim
      best_cmb_by_len[[len_cmb]] <- cmb
    }
  }
  list(best_min_by_len = best_min_by_len,
       best_cmb_by_len = best_cmb_by_len)
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
#' @export
read_xlsx_linux <- function(file) {
  if (jwutil::platformIsWindows())
    stop("can only convert XLSX on linux using xlsx2csv command")

  csvfile <- tempfile()
  on.exit(unlink(csvfile), add = TRUE)
  system(paste0("xlsx2csv --delimiter=tab --dateformat=%m-%d-%y \"",
                file, "\" > ", csvfile))
  utils::read.delim(csvfile)
}

#' @title build simple linear formula from variable names
#' @description build simple linear formula from variable names given by two
#'   character vectors. TODO: allow unquoted names.
#' @param left character vector
#' @param right character vector
#' @return formula
#' @export
buildLinearFormula <- function(left, right) {
  stats::as.formula(
    paste(
      paste(left, collapse = "+"),
      paste(right, collapse = "+"),
      sep = "~")
  )
}

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
  stopifnot(allIsInteger(which))
  stopifnot(length(len) > 0)
  stopifnot(identical(areIntegers(len), TRUE))
  is.element(seq_len(len), which)
}

#' @title recursive remove
#' @description search through environments until the variables in the list
#'   \code{x} are all gone. This doesn't delete functions. No barrier to
#'   infinite recursion, but \code{rm} should be able to delete anything that
#'   \code{exists} can see.
#' @param x variables to annihilate
#' @param envir environment to start at, defaults to calling frame.
#' @export
rm_r <- function(x, envir = parent.frame()) {
  suppressWarnings({
    while (any(vapply(x, exists, logical(1),
                      mode = "numeric", inherits = TRUE, envir = envir)))
      rm(list = x, envir = envir, inherits = TRUE)

  })
}

#' Summarize objects in scope
#'
#' Get type, size (bytes) and dimensions of objects in scope
#' @param pos pos
#' @param pattern regex pattern to match objects of interest
#' @param order.by which column to order by
#' @param decreasing default is \code{TRUE}
#' @param head default is \code{FALSE} but if true, just show top \code{n}
#' @param n number to show if limiting to \code{head}
#' @export
ls.objects <- function(pos = 1, pattern, order.by,
                       decreasing = FALSE, head = FALSE, n = 5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, utils::object.size)
  obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  if (head)
    out <- head(out, n)
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
  ls.objects(..., order.by = "Size", decreasing = TRUE, head = TRUE, n = n)

#' @title is the object a \code{Date}
#' @description copied from lubridate
#' @param x object to test
#' @return logical
#' @export
is.Date <- function(x)
  methods::is(x, "Date")

#' @title extract code from knitr vignette and source it
#' @description extract code from knitr vignette and source it
#' @param input path to file as single character string
#' @param output output file path, defaults to a file in a temporary name based
#'   on \code{input}
#' @param documentation single integer value passed on to \code{knitr::purl}. An
#'   integer specifying the level of documentation to go the tangled script: 0
#'   means pure code (discard all text chunks); 1 (default) means add the chunk
#'   headers to code; 2 means add all text chunks to code as roxygen comments
#' @param ... further parameters passed to \code{source}
#' @export
source_purl <- function(input,
                        output = file.path(tempdir(),
                                           paste0(basename(input), ".R")),
                        documentation = 1L, ...) {
  requireNamespace("knitr")
  checkmate::assertFile(input)
  checkmate::assertPathForOutput(output, overwrite = TRUE)
  checkmate::assertInt(documentation)
  knitr::purl(input, output, quiet = TRUE, documentation = documentation)
  source(output, ...)
}

#' Save given variable in package data directory
#'
#' File is named varname.RData with an optional suffix before .RData
#'
#' @param var_name character or symbol, e.g. "myvar" or \code{myvar}, either of
#'   which would find \code{myvar} in the parent environment, and save it as
#'   \code{myvar.RData} in \code{package_root/data}.
#' @param suffix character scalar
#' @param package_dir character containing the directory root of the package
#'   tree in which to save the data. Default is the current working directory.
#' @param envir environment in which to look for the variable to save
#' @export
save_in_data_dir <- function(var_name, suffix = "",
                             package_dir = getwd(), envir = parent.frame()) {
  checkmate::assertString(suffix)
  var_name <- as.character(substitute(var_name))
  checkmate::assertString(var_name)
  stopifnot(exists(var_name, envir = envir))
  save(list = var_name,
       envir = envir,
       file = file.path(package_dir, "data", strip(paste0(var_name, suffix, ".RData"))),
       compress = "xz")
  message("Now reload package to enable updated/new data: ", var_name)
}

utils::globalVariables(c(".", "%>%"))

#' Find minimum R version required for package
#'
#' Recursively search dependencies for R version, and find the highest stated R
#' version requirement.
#' @source Based on ideas from
#'   http://stackoverflow.com/questions/38686427/determine-minimum-r-version-for-all-package-dependencies
#'
#' @param pkg string with name of package to check
#' @examples
#' base <- c("base", "compiler", "datasets", "grDevices", "graphics",
#' "grid", "methods", "parallel", "profile", "splines", "stats",
#'  "stats4", "tcltk", "tools", "translations")
#'
#' \dontrun{
#' base_reqs <- lapply(base, min_r_version)
#'
#' contrib <- c("KernSmooth", "MASS", "Matrix", "boot",
#' "class", "cluster", "codetools", "foreign", "lattice",
#'  "mgcv", "nlme", "nnet", "rpart", "spatial", "survival")
#'
#' contrib_reqs <- lapply(contrib, min_r_version)
#' }
#' min_r_version("icd")
#' @export
min_r_version <- function(pkg) {
  requireNamespace("tools")
  requireNamespace("utils")
  avail <- utils::available.packages(utils::contrib.url("https://cloud.r-project.org"))
  deps <- tools::package_dependencies(pkg, db = avail, recursive = TRUE)
  if (is.null(deps))
    stop("package not found")

  pkgs <- deps[[1]]
  repo <- getOption("repo")
  if (is.null(repo))
    repo <- "https://cloud.r-project.org"

  matches <- avail[, "Package"] %in% pkgs
  pkg_list <- avail[matches, "Depends"]
  vers <- grep("^R$|^R \\(.*\\)$", pkg_list, value = TRUE)
  vers <- gsub("[^0-9.]", "", vers)
  if (length(vers) == 0)
    return("Not specified")

  max_ver <- vers[1]
  if (length(vers) == 1)
    return(max_ver)

  for (v in 2:length(vers))
    if (utils::compareVersion(vers[v], max_ver) > 0)
      max_ver <- vers[v]

  max_ver
}

#' Fast Factor Generation
#'
#' This function generates factors more quickly, without leveraging
#' \code{fastmatch}. The speed increase with \code{fastmatch} for ICD-9 codes
#' was about 33% reduction for 10 million codes. SOMEDAY could be faster still
#' using \code{Rcpp}, and a hashed matching algorithm.
#'
#' \code{NaN}s are converted to \code{NA} when used on numeric values. Extracted
#' from https://github.com/kevinushey/Kmisc.git
#'
#' These feature from base R are missing: \code{exclude = NA, ordered =
#' is.ordered(x), nmax = NA}
#' @author Kevin Ushey, adapted by Jack Wasey
#' @param x An object of atomic type \code{integer}, \code{numeric},
#'   \code{character} or \code{logical}.
#' @param levels An optional character vector of levels. Is coerced to the same
#'   type as \code{x}. By default, we compute the levels as
#'   \code{sort(unique.default(x))}.
#' @param labels A set of labels used to rename the levels, if desired.
#' @examples
#' \dontrun{
#' pts <- icd:::random_unordered_patients(1e7)
#' u <- unique.default(pts$code)
#' # this shows that stringr (which uses stringi) sort takes 50% longer than
#' # built-in R sort.
#' microbenchmark::microbenchmark(sort(u), str_sort(u))
#'
#' # this shows that \code{factor_} is about 50% faster than \code{factor} for
#' # big vectors of strings
#'
#' # without sorting is much faster:
#' microbenchmark::microbenchmark(factor(pts$code),
#'                                # factor_(pts$code),
#'                                factor_nosort(pts$code),
#'                                times = 25)
#' }
#' @details I don't think there is any requirement for factor levels to be
#'   sorted in advance, especially not for ICD-9 codes where a simple
#'   alphanumeric sorting will likely be completely wrong.
#' @export
factor_nosort <- function(x, levels = NULL, labels = levels) {
  if (is.factor(x)) return(x)
  if (is.null(levels)) levels <- unique.default(x)
  suppressWarnings(f <- match(x, levels))
  levels(f) <- as.character(labels)
  class(f) <- "factor"
  f
}

#' Pipe
#'
#' Use the pipe function, \code{\%>\%} to turn function composition into a
#' series of imperative statements.
#'
#' @importFrom magrittr "%>%" "%<>%" set_names extract2
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @param lhs,rhs chained functions and results
NULL
