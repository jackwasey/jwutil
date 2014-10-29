#' @title check whether character vector represents all numeric values
#' @description check whether all the items of input vector are numeric without
#'   throwing warning derived from Hmsic package
#' @param x is a character vector to be tested
#' @param extras is a vector of character strings which are counted as NA
#'   values, defaults to '.' and 'NA'
#' @return logical
#' @export
allIsNumeric <- function(x, extras=c('.','NA')) {
  old <- options(warn=-1)
  on.exit(options(old))
  xs <- x[x %nin% c('',extras)]
  !any(is.na(as.numeric(xs)))
}

#' @title convert factor or vector to character without warnings
#' @description correctly converts factors to vectors, and then converts to
#'   character, which may silently introduce NAs
#' @param x is a vector, probably of numbers of characters
#' @return character vector, may have NA values
#' @export
asCharacterNoWarn <- function(x) {
  old <- options(warn = -1)
  on.exit(options(old))
  if (class(x) == 'factor') x <- levels(x)[x]
  as.character(x)
}

#' @title convert factor or vector to numeric without warnings
#' @aliases asIntegerNoWarn
#' @description correctly converts factors to vectors, and then converts to
#'   numeric or integer, which may silently introduce NAs. Invisible rounding
#'   errors can be a problem going from numeric to integer, so consider adding
#'   tolerance to this conversion. \code{asIntegerNoWarn} silently
#'   \code{\link{floor}}s.
#' @param x is a vector, probably of numbers of characters
#' @return numeric vector, may have NA values
#' @export
asNumericNoWarn <- function(x) {
  old <- options(warn = -1)
  on.exit(options(old))
  if (class(x)=='factor') x <- levels(x)[x]
  as.numeric(x)
}

#' @rdname asNumericNoWarn
#' @export
asIntegerNoWarn <- function(x) {
  as.integer(asNumericNoWarn(x))
}

#' @rdname asNumericNoWarn
#' @export
areIntegers <- function(x) {
  n <- asNumericNoWarn(x)
  i <- abs(n - floor(n)) < 1e-9
  i[is.na(i)] <- FALSE
  i
}

#' @title inverse of \%in\%
#' @description borrowed from Hmisc. See %in%
#' @param x is the vector of values to be matched
#' @param table is actually a vector, to be matched against
#' @return logical vector of length of x
#' @export
"%nin%" <- function(x, table) match(x, table, nomatch = 0) == 0
# original %in% is: match(x, table, nomatch = 0L) > 0L

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
  unzip(zipfile, exdir = zipdir) # files="" so extract all
  files <- list.files(zipdir)
  if (is.null(filename)) {
    if (length(files) == 1) {
      filename <- files
    } else {
      stop("multiple files in zip, but no filename specified: ", paste(files, collapse = ", "))
    }
  } else { # filename specified
    stopifnot(length(filename) ==1)
    stopifnot(filename %in% files)
  }
  file <- paste(zipdir, files[1], sep="/")
  do.call(FUN, args = c(list(file.path(zipdir, filename)), list(...)))
}

#' @title count non-numeric elements
#' @description counts the number of non-numeric elements in a vector, without throwing warnings
#' @details did have \code{extras = c(".", "NA"))}
#' @param x is usually a charcter vector
#' @return integer
#' @export
countNotNumeric <- function (x) {
  old <- options(warn = -1)
  on.exit(options(old))
  #xs <- x[x %nin% c("", extras)] #%nin% is in Hmisc, and = !%iin%
  countIsNa(asNumericNoWarn(x))
}

#' @title count numeric elements
#' @description counts the number of numeric elements in a vector, without throwing warnings
#' @param x is usually a character vector
#' @return integer
#' @export
countNumeric <- function(x) {
  length(x) - countNotNumeric(x)
}

#' @title count NA in vector
#' @param x vector
#' @return integer
#' @export
countIsNa <- function(x) {
  sum(is.na(x))
}

#' @title Proportion of NA values in a vector
#' @param x is a vector which may have NA values
#' @return numeric proportion of NAs in the supplied vector
#' @export
propIsNa <- function(x) {
  if (length(x) == 0) return(0)
  countIsNa(x) / length(x)
}

#' @title count which combinations of fields have at least one non-NA
#' @description cycles through the given data frame twice, and applies logical OR to all elements of each column
#' it then counts how many of these pairs are not-na, i.e. have at least one non-NA value
#' TODO: tests
#' @param d data.frame
#' @return matrix with nrow and ncol being the number of fields in the given dataframe
#' @export
countNonNaPairs <- function(d) {
  apply(!is.na(d),
        MARGIN = 2,
        FUN = function(y) {
          apply(is.na(d),
                MARGIN = 2,
                FUN = function(x, y) sum(x|y),
                y)
        }
  )
}

#' @title running totals of number of non-NA values in consecutive fields
#' @description counts non-NA fields in first field, then progreses through fields, OR new field and saves running total for each field
#' TODO: tests
#' @param d data.frame
#' @return vector of cumulative non-NA counts with names corresponding to the given data frame
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

#' @title merge lists by names
#' @description merge lists by combining all the elements of the list items with the matching names
#' @param x list with named elements
#' @param y list with named elements
#' @return list
#' @export
mergeLists <- function(x, y) {
  both <- list(x, y)
  n <- unique(unlist(lapply(both, names)))
  names(n) <- n
  lapply(n, function(ni) unlist(lapply(both, `[[`, ni)))
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
#' @description PeriopAIM data comes conveniently with a single date and a load
#'   of integers representing times. This function restores the full date-time.
#'   It does not know if midnight happened before the time was recorded...
#' @param dts vector of dates, in string format \%Y-\%m-\%d or simple R Date
#'   objects
#' @param tms vector of times, i.e. number in range 0 to 2400, as string or
#'   integer, with or without trailing zeros
#' @return vector of POSIXlt date-times
#' @export
add_time_to_date <- function(tms, dts) {

  if (length(dts) != length(tms))
    stop("must have matching lengths of date and time vectors. I got: %d and %d",
         length(dts), length(tms))

  if (class(dts) %nin% c("Date","character") && !is.na(dts))
    stop(paste("date must be of class Date, character, but received: %s",
               class(dts)))

  if (class(dts) == "character" && any(grepl(pattern="\\S\\s\\S", dts)))
    warning("suspect time is given with date, which invalidates this entire function. e.g. %s",
            dts[grepl(pattern="\\S\\s\\S", dts)][1])

  dts <- tryCatch( {
    as.Date(dts) },
    error = function(cond) {
      warning("Date '%s' is ambiguous. (%s). Returning NA...", dts, cond)
      NA
    }
  )

  # a single NA value could appear as type logical
  if (class(tms) %nin% c("numeric","integer","character") && !is.na(tms))
    stop("time must be numeric or character, but got class for times of '%s'.",
         class(tms))

  # this is a data error, not a programming error, so just warn and set NA
  if (any(dts < as.Date("1850-01-01"), na.rm = TRUE)) {
    warning("some dates are before 1850: ", dts[dts<as.Date("1850-01-01")])
    dts[dts < as.Date("1850-01-01")] <- NA
  }

  if (!all(isValidTime(tms))) {
    warning("invalid times received, replacing with NA")
    tms[!isValidTime(tms)] <- NA
  }

  # drop colons, if any
  if (class(tms) == "character")  tms <- gsub(":", "", tms, fixed = TRUE)

  message("working with times:", tms, capture = TRUE)

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
#' @description allow leading and trailing space, optional colon in middle, 2400 is not allowed.
#' @param tms is a vector of characters which may represent times
#' @return logical vector, with NA out if NA given
#' @export
isValidTime <- function(tms) {
  grepl(pattern="^[[:space:]]*([01]?[0-9]|2[0-3])?:?[0-5]?[0-9][[:space:]]*$", tms)
  # Don't do this, or we can't use logical test in case all vals are NA. validTimes[is.na(tms)] <- NA # grepl only gives T or F output
  #TODO: write tests...
}

#' @title trim null or empty values from a list
#' @param x list
#' @return trimmed list
#' @export
listTrim  <-  function(x){   # delele null/empty entries in a list
  x[unlist(lapply(x, length) != 0)]
}

#' @title flatten a list
#' @description unlike unlist, this function returns a list of objects of different data types, but removes any depth
#' @param ... list or any set of objects which will be made into a list, may include lists and nested lists
#' @param na.rm will drop NA values if TRUE
#' @return list without nested lists, objects with preserved data types
#' @source https://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion
#' @export
flatten_list <- function(..., na.rm = FALSE) {
  x <- list(...)
  y <- list()
  rapply(x, function(x) y <<- c(y,x))
  if (na.rm)
    return(y[!is.na(y)])
  y
}
