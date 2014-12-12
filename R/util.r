#' @title check whether character vector represents all numeric values
#' @description check whether all the items of input vector are numeric without
#'   throwing warning derived from Hmsic package
#' @param x is a character vector to be tested
#' @param extras is a vector of character strings which are counted as NA
#'   values, defaults to '.' and 'NA'. Also allow \code{NA}.
#' @return logical scalar
#' @export
allIsNumeric <- function(x, extras = c('.', 'NA', NA)) {
  old <- options(warn = - 1)
  on.exit(options(old))
  xs <- x[x %nin% c('',extras)]
  !any(is.na(as.numeric(xs)))
}

#' @title check whether vector represents all integer values, not that the same
#'   as \code{is.integer}
#' @description check whether all the items of input vector are integer
#'   as.integer
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

#' @title convert factor or vector to character without warnings
#' @description correctly converts factors to vectors, and then converts to
#'   character, which may silently introduce NAs
#' @param x is a vector, probably of numbers of characters
#' @return character vector, may have NA values
#' @export
asCharacterNoWarn <- function(x) {
  old <- options(warn = - 1)
  on.exit(options(old))
  if (is.factor(x)) x <- levels(x)[x]
  as.character(x)
}

#' @title convert factor or vector to numeric without warnings
#' @aliases asIntegerNoWarn
#' @description correctly converts factors to vectors, and then converts to
#'   numeric or integer, which may silently introduce NAs. Invisible rounding
#'   errors can be a problem going from numeric to integer, so consider adding
#'   tolerance to this conversion. \code{asIntegerNoWarn} silently
#'   \code{\link{floor}}s.
#'
#'   "are" functions return a value for each input, where is "allIs" functions
#'   return a single logical.
#' @param x is a vector, probably of numbers of characters
#' @return numeric vector, may have NA values
#' @export
asNumericNoWarn <- function(x) {
  old <- options(warn = - 1)
  on.exit(options(old))
  if (is.factor(x)) x <- levels(x)[x]
  as.numeric(x)
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
  zipfile <- tempfile()
  download.file(url = url, destfile = zipfile, quiet = TRUE)
  zipdir <- tempfile()
  dir.create(zipdir)
  unzip(zipfile, exdir = zipdir)  # files="" so extract all
  files <- list.files(zipdir)
  if (is.null(filename)) {
    if (length(files) == 1) {
      filename <- files
    } else {
      stop("multiple files in zip, but no filename specified: ",
           paste(files, collapse = ", "))
    }
  } else {  # filename specified
    stopifnot(length(filename) == 1)
    stopifnot(filename %in% files)
  }
  file <- paste(zipdir, files[1], sep="/")
  do.call(FUN, args = c(list(file.path(zipdir, filename)), list(...)))
}

#' @title count non-numeric elements
#' @description counts the number of non-numeric elements in a vector, without
#'   throwing warnings
#' @details did have \code{extras = c(".", "NA"))}
#' @param x is usually a charcter vector
#' @return integer
#' @export
countNotNumeric <- function (x)
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
#' @param x vector
#' @return integer
#' @export
countIsNa <- function(x)
  sum(is.na(x))

#' @title Proportion of NA values in a vector
#' @param x is a vector which may have NA values
#' @return numeric proportion of NAs in the supplied vector
#' @export
propIsNa <- function(x)
  ifelse(length(x) == 0, 0, countIsNa(x) / length(x))

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
          assign("running", running | x, envir=envir)
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

  if (class(dts) %nin% c("Date","character") && !is.na(dts))
    stop(paste("date must be of class Date, character, but received: %s",
               class(dts)))

  # if a time part is given in the date field, this is an error
  if (is.character(dts) && any(grepl(pattern="\\S\\s\\S", dts)))
    stop("suspect time is already given with date argument, \
         which invalidates this entire function. e.g. %s",
         dts[grepl(pattern="\\S\\s\\S", dts)][1])

  # convert to Date (may already be Date, but that's fine) any conversion error
  # in any item will result in an error. an alternative strategy would be to
  # individually tryCatch converting each Date, returning warnings, NA, or
  # detailed error message. TODO
  dts <- as.Date(dts)

  # a single NA value could appear as type logical
  if (class(tms) %nin% c("numeric","integer","character") && !is.na(tms))
    stop("time must be numeric or character, but got class for times of '%s'.",
         class(tms))

  # this is a data error, not a programming error, stop
  if (any(dts < as.Date("1850-01-01"), na.rm = TRUE)) {
    stop("some dates are before 1850: ", dts[dts < as.Date("1850-01-01")])
    # could alternatively set NA, warn and continue:
    # dts[dts < as.Date("1850-01-01")] <- NA
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
  badRange <- any(tms < 0 || tms > 2359)
  if (!is.na(badRange) && badRange)  {
    warning("invalid times found. Setting to NA:", tms, capture=T)
    tms[badRange] <- NA
  }

  timesfourzeros <- formatC(tms, width=4, format="d", flag="0")
  strptime(paste(dts, timesfourzeros, sep=" "), "%Y-%m-%d %H%M")
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
  #Don't do this, or we can't use logical test in case all vals are NA.
  #validTimes[is.na(tms)] <- NA # grepl only gives T or F output TODO: write
  #tests...
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
  stopifnot(length(x) < 13)  # factorial
  # break out of recursion:
  if (length(x) == 2) return(rbind(x, c(x[2], x[1])))

  res <- c()

  #take each one and place it first, then recurse the rest:
  for (element in 1:length(x)) {
    sub_combs <- Recall(x[ - element])  # recurse
    new_combs <- cbind(x[element], sub_combs)
    res <- rbind(res, new_combs)
  }
  unname(res)
}

#' Are we running on Linux or Windows?
#'
#' @return logical
#' @export
platformIsLinux <- function()
  Sys.info()[["sysname"]] == "Linux"

#' @rdname platformIsLinux
#' @export
platformIsWindows <- function()
  Sys.info()[["sysname"]] == "Windows"

#' @title read .xlsx file, interpret as CSV, and return data frame
#' @description currently relies on Linux xlsx2csv command, but could
#'   potentially be done with VB script in Windows
#' @param file is the path to the .xlsx file
#' @return data frame
#' @export
readXlsxLinux <- function(file) {
  if (jwutil::platformIsWindows())
    stop("can only convert XLSX on linux using xlsx2csv command")

  csvfile <- tempfile()
  system(paste0('xlsx2csv --delimiter=tab --dateformat=%m-%d-%y "',
           file, '" > ', csvfile))
  read.delim(csvfile)
}

#' @title build simple linear formula from variable names
#' @param left character vector
#' @param right character vector
#' @return formula
#' @export
buildLinearFormula <- function (left, right) {
  as.formula(
    paste(
      paste(left, collapse = "+"),
      paste(right, collapse = "+"),
      sep="~")
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
  stopifnot(all(which > 0) )
  stopifnot(allIsInteger(which))
  stopifnot(length(len) > 0)
  stopifnot(identical(areIntegers(len), TRUE))
  is.element(seq_len(len), which)
}
