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
  logicalFields <- names(dframe)[sapply(dframe,class) == 'logical']
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

#' @title convert factor into a data.frame of logicals
#' @description converts a single factor into a data.frame with multiple T/F
#'   fields, one for each factor
#' @param fctr factor
#' @param prefix defaults to "f" to pre-pend the factor level when constructing
#'   the data frame columns names
#' @param sep scalar character, introduced between factor names and levels when
#'   forming new data frame column names
#' @param na.rm logical scalar: if NA data and/or NA levels, then covert to NA
#'   strings and expand these as for any other factor
#' @template verbose
#' @return data.frame with columns of logicals
#' @export
factorToDataframeLogical <- function(fctr,
                                     prefix = deparse(substitute(fctr)),
                                     sep = "",
                                     na.rm = TRUE,
                                     verbose = FALSE) {
  stopifnot(is.factor(fctr))
  stopifnot(is.character(prefix))
  stopifnot(length(prefix) == 1)
  stopifnot(length(sep) == 1)
  stopifnot(is.logical(na.rm))
  stopifnot(length(na.rm) == 1)
  stopifnot(is.logical(verbose))
  stopifnot(length(verbose) == 1)
  if (verbose && sum(is.na(fctr)) > 0)
    warning("factorToCols: factor passed to factorCols contains NA")
  #remove unused factor levels
  fctr <- factor(fctr)
  stopifnot(length(levels(fctr)) > 0)
  stopifnot(length(fctr) > 0)

  if (na.rm) {
    # don't ignore NA values or levels
    fctr <- factor(fctr, unique(fctr), exclude = NULL)
    levels(fctr)[is.na(levels(fctr))] <- "NA"
  }

  if (length(levels(fctr)) == 1) {
    if (verbose) message("only one factor level, returning all TRUE")
    df <- data.frame(fctr)
    names(df) <- prefix
    return(df)
  }

  if (length(levels(fctr)) == 2) {
    if (verbose)
      message("two factor levels: returning TRUE/FALSE for first level")
    df <- data.frame(fctr == levels(fctr)[1])
    names(df) <- paste(prefix, levels(fctr)[1], sep = sep)
    return(df)
  }

  # set-up data frame with empty logical
  df <- data.frame(tmp = logical(length = length(fctr)))

  if (verbose) message("more than two factor levels")
  for (lev in levels(fctr)) {
    newColName <- paste(prefix, lev, sep = sep)
    if (verbose) message(sprintf("creating new column name: %s", newColName))
    df[newColName] <- fctr == lev
  }
  df["tmp"] <- NULL
  df
}

#' @title Takes factors from a data frame and converts them to true/false fields
#'   with appropriately named fields.
#' @description For a two level factor, this is relatively easy, since we just
#'   replace the field with \code{x==levels(x)[1]} or something like that, and
#'   rename the field to indicate that TRUE is level 1 of the factor. This works
#'   well for gender. For multi-level factors there is redundancy with multiple
#'   new fields now containing FALSE, with only one TRUE for the matching level.
#' @param dframe data.frame to search for factors to convert
#' @param considerFactors character vector of field names in the data frame to
#'   consider. Defaults to all fields
#' @param sep character scalar used to separate field prefixes from factor
#'   values in new column names
#' @param na.rm logical scalar: if NA data and/or NA levels, then covert to NA
#'   strings and expand these as for any other factor
#' @template verbose
#' @return data.frame with no factors
#' @export
expandFactors <- function (dframe,
                           considerFactors = names(dframe),
                           sep = "",
                           na.rm = TRUE,
                           verbose = FALSE) {
  if (verbose) message("converting factors in data frame into logical vectors")

  #message("considerFactors: %s", paste(considerFactors, collapse=', '))

  # identify which of the last of fields is actually a factor
  factorNames <- getFactorNames(dframe, considerFactors)

  #message("got factorNames: %s", paste(factorNames, collapse=", "))

  if (length(factorNames) > 0) {
    if (verbose) message("there are factors to be converted into values: ",
                         paste(factorNames, collapse = ", "))
    for (fn in factorNames) {
      if (verbose) message(sprintf("working on factor: %s", fn))
      dfpart <- factorToDataframeLogical(
        fctr = dframe[[fn]], prefix = fn, sep = sep,
        na.rm = na.rm, verbose = verbose)
      dframe[fn] <- NULL
      dframe <- cbind(dframe, dfpart)
    }
  } else {
    if (verbose) message("no factors found to convert in exFactor")
  }
  dframe
}

#' @title get names of the factor fields in a data frame
#' @param dframe data frame
#' @param considerFactors character vector of field names, default is to use all
#'   of them.
#' @return vector
#' @export
getFactorNames <- function(dframe, considerFactors = names(dframe)) {
  if (length(names(dframe)) <= 0) {
    warning("getFactorNames: empty data frame passed in. Returning NULL.")
    return()
  }
  if (length(considerFactors) <= 0) {
    warning("getFactorNames: empty considerFactors. Returning NULL.")
    return()
  }

  considerFactors[sapply(dframe[1, considerFactors], class) == "factor"]
  #if (anyDuplicated) #TODO
}

#' @rdname getFactorNames
#' @export
getNonFactorNames <- function(dframe, considerFactors = names(dframe)) {
  considerFactors[considerFactors %nin% getFactorNames(dframe, considerFactors)]
}

#' @title get NA field names from data frame
#' @param dframe data.frame
#' @return vector of names of fields which contain any NA values, length zero if
#'   no matches
#' @export
getNAFields <- function(dframe) {
  if (class(dframe) != "data.frame")
    stop(paste("getNAfields: passed an object of class: ",
               class(dframe), collapse=" "))
  naFields <- names(dframe)[sapply(dframe, countIsNa) > 0]
  if (length(naFields) == 0) return(character())
  naFields
}

#' @rdname getNAFields
#' @export
getNonNAFields <- function(dframe)
  names(dframe)[names(dframe) %nin% getNAFields(dframe)]

#' @title return proportion of NA values per field
#' @param dframe is a data frame
#' @return numeric vector
#' @export
propNaPerField <- function(dframe)
  sapply(dframe, function(v) {
    countIsNa(v) / length(v)
  })

#' @title drops rows with NA values in specified fields
#' @description employs complete.cases which is fast internal C code. Returns a
#'   data frame with unused factor levels dropped (these may have been
#'   introduced by dropping rows with some NA values)
#' @param x data frame
#' @param fld vector with names of fields which must have no NA values
#' @template verbose
#' @return data frame without rows containing NA in the specified data fields.
#'   There may be NA values in the resulting data frame in fields which are not
#'   listed in fld.
dropRowsWithNAField <- function(x, fld = names(x), verbose = FALSE) {
  if (verbose) message(sprintf("checking fields: %s for NA values", fld))
  stopifnot(class(fld) == "character")
  stopifnot(class(x) == "data.frame")
  cc <- complete.cases(x[fld])
  droplevels(x[cc,])
}

#' @title merge better
#' @description apply built-in R merge but with additional features:
#' @param x data frame
#' @param y data frame
#' @param by.x field in x to merge on. Unlike \code{merge}, this is compulsory.
#' @param by.y field in y to merge on. Unlike \code{merge}, this is compulsory.
#' @param all.x outer join to keep all x values
#' @param all.y outer join to keep all y values
#' @param affix either prefix or suffix to disambiguate files. By default, this
#'   is the name of the table specified in \code{y}. In all other respects in
#'   this function, \code{x} and \code{y} are symmetric.
#' @param renameConflict - determines whether prefix or suffix is added to
#'   disambiguate conflicting column names. Value can be "suffix", "prefix".
#'   Suffix is the default.
#' @param renameAll - regardless of column name clashes, "prefix" or "suffix"
#'   with every field with original table name, or "no" for neither
#' @param convertFactors Default is TRUE which causes factors to be converted to
#'   character before merge. This is almost certainly safer.
#' @param verbose logical
#' @return merged data frame
#' @export
mergeBetter <- function(x, y, by.x, by.y,
                        all.x = FALSE, all.y = FALSE,
                        affix = NULL,
                        renameConflict = c("suffix", "prefix"),
                        renameAll = c("no", "suffix", "prefix"),
                        convertFactors = TRUE,
                        verbose = FALSE) {

  renameConflict <- match.arg(renameConflict)
  renameAll <- match.arg(renameAll)

  stopifnot(class(x) == "data.frame")
  stopifnot(class(y) == "data.frame")
  stopifnot(length(by.x) == 1, length(by.y) == 1)
  stopifnot(length(all.x) == 1, length(all.y) == 1)
  stopifnot(length(verbose) == 1, length(convertFactors) == 1)


  # we don't want case sensitive names: we rely on case insensitivity, and it is
  # very common for the same data to have case-changes in the field name.
  stopifnot(all(!duplicated(tolower(names(x)))))
  stopifnot(all(!duplicated(tolower(names(y)))))

  # guess a good affix. If y is not just a variable name, use 'y'
  if (is.null(affix)) {
    affix <- deparse(substitute(y))
    if (length(substitute(y)) > 1) affix <- "y"
  }

  #convert factors of keys only # TODO: as.integer may be appropriate
  #sometimes/often. TODO: tests for this
  if (convertFactors) {
    if (class(x[[by.x]]) == "factor")
      x[[by.x]] <- asCharacterNoWarn(x[[by.x]])

    if (class(y[[by.y]]) == "factor")
      y[[by.y]] <- asCharacterNoWarn(y[[by.y]])
  }

  if (verbose) {
    # this informational step could itself be slow in a big merge
    rightMergeDrops <- sum(!(x[[by.x]] %in% y[[by.y]]))
    leftMergeDrops <- sum(!(y[[by.y]]) %in% x[[by.x]])
    if (leftMergeDrops > 0 || rightMergeDrops > 0) {
      message(sprintf("would drop %d out of %d from new table", leftMergeDrops, nrow(y)))
      message(sprintf("and %d out of %d from old table", rightMergeDrops, nrow(x)))
    } else {
      message("no rows will be dropped in the merge - keys match exactly.")
      message("There may still be data differences in the two data frames.")
    }
  }

  # find duplicate field names, ignoring the field we are merging on.
  dupeNames_x <- names(x)[tolower(names(x)) %in% tolower(names(y)) &
                            tolower(names(x)) != tolower(by.x) &
                            tolower(names(x)) != tolower(by.y)]
  if (verbose) message("got duplicate x field names: ", paste(dupeNames_x, collapse=", "))


  # now i want to drop identical fields unless an explicit rename has been requested.

  if (length(dupeNames_x) > 0 && renameAll == "no") {
    if (verbose) message("conflicting field names in the merge but no renaming was
                         requested so using renameConflict to guide the renaming
                         of clashing fields in x: ", paste(dupeNames_x, collapse = ", "))
    dropFields <- c()
    for (xdup in dupeNames_x) {
      #rematch y - this is unsatisfying but simplifies the logic.
      match_x_in_y <- match(xdup, names(y))
      stopifnot(length(match_x_in_y) == 1)  # this means there were two conflicts with that name!
      ydup <- names(y)[match_x_in_y]
      if (verbose) message("checking whether '", xdup,
                           "' (matching '", ydup, "') has duplicated data.")
      isAllEqual <- all.equal(x[[xdup]], y[[ydup]])
      if (identical(isAllEqual, TRUE)) {  # all.equal returns true or a char vector
        if (verbose) message("will drop  identical field: ", ydup)
        dropFields <- c(dropFields, ydup)
      } else {
        if (verbose) message("renaming conflicting field (data is not identical")
        names(y)[which(names(y) == ydup)] <- affixFields(fieldNames = ydup, affix = affix,
                                                         renameHow = renameConflict)
      }
    }
    # actually drop the fields: best not to do while looping through the data frames.
    for (dropField in dropFields) y[dropField] <- NULL
  } else if (renameAll != "no") {
    names(y) <- affixFields(fieldNames = names(y), skipFields = by.y,
                            affix = affix, renameHow = renameAll)
  }
  # end if there are duplicates (no else - we can proceed)

  if (verbose) message(sprintf("merging using id field: %s, and new id field: %s", by.x, by.y))

  m <- merge(x = x, by.x = by.x, all.x = all.x,
        y = y, by.y = by.y, all.y = all.y)
  message(anyDuplicated(names(m)))
  stopifnot(anyDuplicated(names(m)) == 0)
  m
}

#' @title update a set of data frame field names
#' @description prefix or suffix
#' @param fieldNames char vector
#' @param affix character
#' @param skipFields char vector, defaults to include all fields
#' @param renameAll should be "suffix" or "prefix"
#' @param sep default '.'
#' @return character vector, same length as fieldNames
#' @export
affixFields <- function(fieldNames, affix, skipFields = NULL,
                        renameHow = c("suffix", "prefix"),
                        sep = ".") {

  stopifnot(length(affix) == 1)
  stopifnot(nchar(affix) > 0)
  stopifnot(is.null(skipFields) || is.character(skipFields))

  renameHow <- match.arg(renameHow)
  if (renameHow == "suffix") {
    fieldNames[fieldNames %nin% skipFields] <-
      paste(fieldNames[fieldNames %nin% skipFields], affix, sep = sep)
  } else {
    fieldNames[fieldNames %nin% skipFields] <-
      paste(affix, fieldNames[fieldNames %nin% skipFields], sep = sep)
  }
  fieldNames
}

#' @title get items or numerics that would be dropped in a merge
#' @description converts both vectors to numeric. This simulates merging when
#'   one key is character (but contains integer numbers), and another key is
#'   stored as integer.
#' @param x vector or factor
#' @param y vector or factor
#' @return list of two vectors
#' @export
getDropped <- function(x, y)
  list(
    missing_from_x = y[y %nin% x],
    missing_from_y = x[x %nin% y]
  )

#' @title drop duplicate fields
#' @description compares all data in each field to every other field, and drops
#'   the latter match. Will find multiple matches. Doesn't do any type
#'   conversions yet. This is purely by content, not by field name.
#' @param df data.frame
#' @template verbose
#' @return data frame without duplicate fields
#' @export
dropDuplicateFields <- function(df, verbose = FALSE) {
  stopifnot(class(verbose) == "logical" && length(verbose) == 1)
  dropNames <- c()

  # to hell with vectorization
  for (f in 1:(dim(df)[2] - 1)) {
    for (g in (f + 1):dim(df)[2]) {
      if (identical(all.equal(df[[f]], df[[g]]), TRUE)) {
        if (verbose) message(sprintf("found matching fields %s and %s. Dropping one.",
                                     names(df)[f], names(df)[g]))
        dropNames <- c(dropNames, names(df)[g])
      }
    }
  }

  for (dn in dropNames) df[dn] <- NULL
  df
}
