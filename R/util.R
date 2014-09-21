#' @title encode TRUE as 1, and FALSE as 0 (integers)
#' @description when saving data as text files for distribution, printing large
#'   amounts of text containing TRUE and FALSE is inefficient. Convert to binary
#'   takes more R memory, but allows more compact output TODO: test
#' @param dframe dataframe which may contain logical fields
#' @return dframe without logical fields
#' @keywords manip
#' @export
logicalToBinary <- function(dframe) {

  if (class(dframe) != 'data.frame')
    stop("logicalToBinary expects a data frame, but got %s", class(dframe))
  if (any(dim(dframe) == 0))
    stop("got zero in at least one dimension in data frame. %d, %d",
         dim(dframe)[1], dim(dframe)[2])

  # can condense this code into a one-liner, but this is clearer:
  logicalFields <- names(dframe)[sapply(dframe,class)=='logical']
  if (is.na(logicalFields) || length(logicalFields) == 0) return(dframe)

  #update just the logical fields with integers
  dframe[,logicalFields] <-
    vapply(
      X         = dframe[, logicalFields],
      FUN       = function(x) ifelse(x, 1L, 0L),
      FUN.VALUE = integer(length = dim(dframe)[1])
    )
  dframe
}

#' @title strip all whitespace
#' @description could do this with regular expression, but slow, and this
#'   function is called frequently. My only use case works with removal of all
#'   space character whitespace, and I don't expect <TAB>.
#' @param x is a character vector to strip
#' @return character vector
#' @export
strip <- function (x, pattern = " ") {
  # beware unicode
  gsub(pattern = pattern, replacement = "", x, fixed = TRUE, useBytes = TRUE)
}

#' @title strip whitespace from ends of each string in given character vector
#' @description slower than \code{strip}.
#' @param x is a character vector to trim
#' @return character vector
#' @export
trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

#' @title return the actual matches from a bracketed regex
#' @description Be careful: this may throw funny results for exotic regex, but
#'   so far, it seems okay. it also drops the first result which always seems to
#'   be a duplicate or whole-string match
#' @param pattern regular expression: if it has bracketed sections, these
#'   submatches are returned
#' @param text is the string to match against. This vector should be the same
#'   length as the pattern vector, or the patern vector should be length one.
#' @param ... are additional parameters passed to regexec and regmatches. I
#'   haven't tried this: it may need two separate variables containing lists of
#'   params, since this will send everything to both functions.
#' @param dropEmpty logical whether to drop rows with no matches
#' @return list of character vectors, list length being the length of the inptu
#'   text vector.
#' @export
strMultiMatch <- function(pattern, text, dropEmpty = FALSE, ...) {
  # unlist puts the name in the first position, which I don't think I ever want.
  result <- lapply(
    text, function(x) unlist(
      regmatches(
        x = x,
        m = regexec(
          pattern = pattern,
          text=x, ...),
        ...)
    )[-1]
  )
  if (!dropEmpty) return(result)
  result[sapply(result, function(x) length(x) != 0)]
}

#' @rdname strMultiMatch
#' @description \code{strPaitMatch} differs in that there should only be two
#'   pairs of parenthesis, then the first (by default) becomes the name, and the
#'   second the value.
#' @param swap logical scalar, whether to swap the names and values. Default is
#'   not to swap, so the first match becomes the name.
#' @export
strPairMatch <- function(pattern, text, swap = FALSE, dropEmpty = FALSE, ...) {
  res <- strMultiMatch(pattern = pattern, text = text, dropEmpty = TRUE, ...)
  outNames <- vapply(X = res, FUN = '[', FUN.VALUE = character(1), ifelse(swap, 2, 1))
  out <- vapply(X = res, FUN = '[', FUN.VALUE = character(1), ifelse(swap, 1, 2))
  names(out) <- outNames
  out
}

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

#' @title convert factor into a data.frame of logicals
#' @description converts a factor into a data.frame with multiple T/F fields,
#'   one for each factor
#' @param fctr factor
#' @param prefix defaults to "f" to pre-pend the factor level when constructing
#'   the data frame columns names
#' @return data.frame with columns of logicals
#' @export
factorToCols <- function(fctr, prefix = "f") {

  if (is.null(fctr)) { stop("factorToCols: NULL passed instead of factor") }
  if (class(fctr) != "factor") { stop(paste("input data is class ", class(fctr))) }
  if (sum(is.na(fctr))>0) { warning("factorToCols: factor passed to factorCols contains NA") }

  #remove unused factor levels (because we have already taken a limited number
  #of rows of whole data set) message('remove unused factor levels')
  fctr <- factor(fctr)

  if (length(levels(fctr))<=0) { stop(paste("factor has no levels after reducing: ", length(levels(fctr)))) }
  if (length(fctr)<=0) { stop(paste("factor length is: ", length(fctr))) }

  newdframe <- data.frame(fctr)
  names(newdframe) <- c('tempdeleteme')

  message("looping to extract logical vectors from the provided factor")
  for (i in 1:length(levels(fctr))) {
    newColName = paste(prefix, levels(fctr)[i], sep=".")
    #message("factorToCols: creating new columns name: %s", newColName)
    newdframe[,newColName] <- (fctr == levels(fctr)[i])
  }
  newdframe['tempdeleteme'] <- NULL
  newdframe
}

#' @title convert multi level factor to multiple true/false fields
#' @param dframe data.frame to search for factors to convert
#' @param considerFactors character vector of field names in the data frame to
#'   consider. Defaults to all fields
#' @return data.frame with factors converted to multiple binary fields
#' @export
expandFactors <- function (dframe, considerFactors = names(dframe)) {
  message("exFactor: converting factors in a data frame into logical vectors")

  #message("considerFactors: %s", paste(considerFactors, collapse=', '))

  # identify which of the last of fields is actually a factor
  factorNames = getFactorNames(dframe, considerFactors)

  #message("got factorNames: %s", paste(factorNames, collapse=", "))

  newCols <- vector()
  if (length(factorNames)>0) {
    message("there are factors to be converted into values", paste(factorNames, collapse=", "))
    for (mf in 1:length(factorNames)) {
      mfName = factorNames[mf]
      # simplify problem by removing unused factor levels (because we have
      # already taken a limited number of rows of whole data set) remove unused
      # factor levels
      dframe[,mfName] <- factor(dframe[,mfName])
      levelNames = levels(dframe[,mfName]) # now get the reduced list of factor levels
      if (length(levelNames)>0) {
        for (i in 1:length(levelNames)) {
          # create new field in the data frame call "factorname.levelname"
          newColName = paste(mfName, ".", levelNames[i], sep="")
          # and populate it with the boolean whether the data points match the
          # currently considered 'levelname' within the factor
          dframe[,newColName] <- dframe[,mfName] == levels(dframe[,mfName])[i]
          # do i care about keeping list of new columns?
          newCols[length(newCols)+1] <- newColName
        }
      }
      # delete the original factor from the data frame
      dframe[,mfName]<-NULL
    }
  } else {
    message("no factors found to convert in exFactor")
  }
  if (length(newCols) == 0) newCols <- NULL

  message("newFields: %s", paste(newCols, collapse=", "))

  list(dat = dframe, newFields = newCols)
}

# getFactorStatus <- function(dframe, considerFactors=names(dframe)) {
#
#   f <- list()
#   f$factorNames    <- considerFactors[sapply(dframe[1,considerFactors], class)=="factor"]
#   f$nonFactorNames <- considerFactors[sapply(dframe[1,considerFactors], class)!="factor"]
#   return(f)
# }
#

#' @title get names of the factor fields in a data frame
#' @param dframe data frame
#' @param considerFactors character vector of field names, default is to use all
#'   of them.
#' @return vector
#' @export
getFactorNames <- function(dframe, considerFactors = names(dframe)) {
  if (length(names(dframe)) <= 0) { warning("getFactorNames: empty data frame passed in. Returning NULL."); return(NULL)}
  if (length(considerFactors) <= 0) { warning("getFactorNames: empty considerFactors. Returning NULL."); return(NULL)}

  considerFactors[sapply(dframe[1, considerFactors], class) == "factor"]
  #if (anyDuplicated) #TODO
  #if (length(factorNames)<=0) { message("getFactorNames: found no factors. Returning NULL.") ; return(NULL)}
}

#' @rdname getFactorNames
#' @export
getNonFactorNames <- function(dframe, considerFactors = names(dframe)) {
  considerFactors[considerFactors %nin% getFactorNames(dframe, considerFactors)]
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

#' @title check whether character vecotr represents all numeric values
#' @description check whether all the items of input vector are numeric without throwing warning
#' derived from Hmsic package
#' @param x is a character vector to be tested
#' @param extras is a vector of character strings which are counted as NA values, defaults to '.' and 'NA'
#' @return logical
#' @export
allIsNumeric <- function(x, extras=c('.','NA')) {
  old <- options(warn=-1)
  on.exit(options(old))
  #jack: TODO: this could be a single regex, or even non-regex sub
  x <- sub('[[:space:]]+$', '', x)
  x <- sub('^[[:space:]]+', '', x)
  xs <- x[x %nin% c('',extras)]
  !any(is.na(as.numeric(xs)))
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

#' @title get NA field names from data frame
#' @param dframe data.frame
#' @return vector of names of fields which contain any NA values, or NULL if none
#' @export
getNAFields <- function(dframe) {
  if (class(dframe) != "data.frame") stop(paste("getNAfields: passed an object of class: ", class(dframe), collapse=" "))
  naFields <- names(dframe)[sapply(dframe, countIsNa)>0]
  if (length(naFields)>0) return(naFields)
}

#' @title return proportion of NA values per field
#' @param dframe is a data frame
#' @return numeric vector
#' @export
propNaPerField <- function(dframe) {
  sapply(dframe, function(v) {
    countIsNa(v) / length(v)
  })
}

#' @title drops rows with NA values in specified fields
#' @description unlike na.omit, the list of fields determines exactly which fields must have no NA values
#' @param dat is a data frame
#' @param ... names of fields which must have no NA values
#' @return data frame without rows containing NA in the specified data fields.
#' @export
dropRowsWithNAField <- function(dat, ...) {
  fld <- c(..., recursive=TRUE)
  message(fld)

  c(is.na(dat))
  for (f in fld) {
    dat <- dat[as.vector(!is.na(dat[f])),]
  }
  # automatically renumber the rows - we don't care about the internal R numbering as we have other unique identifiers per rwo
  row.names(dat) <- NULL
  droplevels(dat)
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

#' @title save data compressed in data folder
#' @description xz appears to fail on Windows, so use bzip2
#'
#' tools::checkRdaFiles(file.path("data", list.files(path = "data")))
#' tools::resaveRdaFiles(file.path("data", list.files(path = "data")), compress = "xz")
#' @param varName char name of the variable in the calling frame to save
#' @param suffix char additional characters before ".RData"
#' @export
saveInDataDir <- function(varName, suffix) {
  save(list = varName,
       envir = parent.frame(),
       file = file.path('data', paste0(varName, suffix, '.RData')),
       compress = "bzip2"
  )
}


#' @title merge better
#' @description apply built-in R merge but with additional features:

#' @param leftOuterJoin is logical. If true, all the patcoms in the provided
#'   'dat' data frame are retained regardless of whether these patcoms exist in
#'   the listed tables.
#' @param ifConflict - determines whether prefix or suffix is added to
#'   disambiguate conflicting column names. Value can be "suffix" (default) or
#'   "prefix"
#' @param affix either prefix or suffix to disambiguate files. typically this is
#'   the source table name
#' @param doRename - regardless of column name clashes, "prefix" or "suffix"
#'   with every field with original table name, or "no" for neither 1. check
#'   whether matching name fields are identical, and if so, don't duplicate in
#'   merge output 2. more sophisticated renaming of conflicting fields 3.
#'   logging of the overlap between the data sets 4. limit just to left outer
#'   join
#' @return merged data frame
#' @export
mergeBetter <- function(x, y,
                        by.x = "patcom", by.y = "patcom",
                        all.x = FALSE,
                        all.y = FALSE,
                        affix = deparse(substitute(x)),
                        ifConflict = c("suffix", "prefix"),
                        doRename = c("no", "suffix", "prefix")) {

  ifConflict <- match.arg(ifConflict)
  doRename <- match.arg(doRename)

  rightMergeDrops <- sum(!(x[[by.x]] %in% y[[by.y]]))
  leftMergeDrops <- sum(!(y[[by.y]]) %in% x[[by.x]])
  if (leftMergeDrops>0 | rightMergeDrops>0) {
    message(name="jh.mergeBetter", paste("mergeBetter: would drop ",leftMergeDrops, " out of ", nrow(y),
                                         " from the new table, and  ", rightMergeDrops, "out of ", nrow(x),
                                         " from the existing data."))
  } else {
    message(name="jh.mergeBetter", "no rows will be dropped in the merge - keys match exactly")
  }

  # find duplicate field names
  duplicateFieldNames <- names(x)[ names(x) %in% names(y) & !names(x) == by.y ]

  if (length(duplicateFieldNames)>0 && doRename=="no") {
    message("there are conflicting field names in the merge but no prefix or suffix was requested:", duplicateFieldNames)
    for (n in duplicateFieldNames) {
      if (identical(x[n], y[n])) {
        y[n] <- NULL # drop the field if it is identical to another one with the same name
        message(name="jh.mergeBetter", "dropping identical field: ", n) # and warn?
      } else { # rename individual conflicting fields
        if (ifConflict == "suffix") newName <- paste(n, affix, sep=".") else newName <- paste(affix, n, sep=".")
        names(y)[which(names(y) == n)] <- newName
      }
    }
  }

  names(y) <- affixFields(names(y), by.y, affix, doRename)

  #sprintf(name="jh.mergeBetter", "merging table '%s' using merged id field: %s, and new id field: %s", t, by,x, by.y)
  merged <- merge(
    x = x,
    y = y,
    by.x = by.x,
    by.y = by.y,
    all.x = all.x,
    all.y = all.y,
    suffixes = c("",paste0(".",fix)) # if we didn't already rename, then allow merge itself to rename if conflict.
  )
  #sprintf(name="jh.mergeBetter", "merged names after merge: %s", names(merged), capture=T)
  merged
}

#' @title update a set of data frame field names
#' @description prefix or suffix
#' @param fieldNames char vector
#' @param skipFields char vector
#' @param affix character
#' @param doRename should be "suffix" or "prefix"
#' @param sep default '.'
#' @return character vector, same length as fieldNames
#' @export
affixFields <- function(fieldNames, skipFields, affix, doRename, sep=".") {

  if (doRename == "suffix") {
    message("renaming first table field names with suffix")
    fieldNames[fieldNames %nin% skipFields] <- paste(fieldNames[fieldNames %nin% skipFields], affix, sep=sep)
  } else if (doRename == "prefix") {
    message("renaming first table field names with prefix")
    fieldNames[fieldNames %nin% skipFields] <- paste(affix, fieldNames[fieldNames %nin% skipFields], sep=sep)
  } else {
    message(name="not adding prefix or suffix to first table because doRename = ", doRename)
  }
  fieldNames
}

#' @title get numbers that would be dropped in a merge
#' @description converts both vectors to numeric
#' @param one vector or factor
#' @param two vector or factor
#' @return list of two vectors
#' @export
getDroppedNumeric <- function(one, two) {
  one <- asNumericNoWarn(one)
  two <- asNumericNoWarn(two)
  list(
    missingFromOne = two[two %nin% one],
    missingFromOne = one[one %nin% two])
}
