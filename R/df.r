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
  if (verbose && sum(is.na(fctr)) > 0) warning("factorToCols: factor passed to factorCols contains NA")
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
    if (verbose) message("two factor levels: returning TRUE/FALSE for first level")
    df <- data.frame(fctr == levels(fctr)[1])
    names(df) <- paste(prefix, levels(fctr)[1], sep = sep)
    return(df)
  }

  # set-up data frame with empty logical
  df <- data.frame(tmp = logical(length = length(fctr)))

  if (verbose) message("more than two factor levels")
  for (lev in levels(fctr)) {
    newColName = paste(prefix, lev, sep = sep)
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
  if (verbose) message("exFactor: converting factors in a data frame into logical vectors")

  #message("considerFactors: %s", paste(considerFactors, collapse=', '))

  # identify which of the last of fields is actually a factor
  factorNames = getFactorNames(dframe, considerFactors)

  #message("got factorNames: %s", paste(factorNames, collapse=", "))

  if (length(factorNames) > 0) {
    if (verbose) message("there are factors to be converted into values: ",
                         paste(factorNames, collapse = ", "))
    for (fn in factorNames) {
      if (verbose) message(sprintf("working on factor: %s", fn))
      dfpart <- factorToDataframeLogical(fctr = dframe[[fn]],
                                         prefix = fn, sep = sep, na.rm = na.rm, verbose = verbose)
      dframe[fn] <- NULL
      dframe <- cbind(dframe, dfpart)
    }
  } else {
    if (verbose) message("no factors found to convert in exFactor")
  }
  dframe
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

#' @title get NA field names from data frame
#' @param dframe data.frame
#' @return vector of names of fields which contain any NA values, length zero if no matches
#' @export
getNAFields <- function(dframe) {
  if (class(dframe) != "data.frame") stop(paste("getNAfields: passed an object of class: ", class(dframe), collapse=" "))
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
propNaPerField <- function(dframe) {
  sapply(dframe, function(v) {
    countIsNa(v) / length(v)
  })
}

#' @title drops rows with NA values in specified fields
#' @description unlike na.omit, the list of fields determines exactly which fields must have no NA values
#' @param dat is a data frame
#' @param ... names of fields which must have no NA values
#' @template verbose
#' @return data frame without rows containing NA in the specified data fields.
#' @export
dropRowsWithNAField <- function(dat, ..., verbose = FALSE) {
  fld <- c(..., recursive = TRUE)
  if (verbose) message(fld)

  c(is.na(dat))
  for (f in fld) {
    dat <- dat[as.vector(!is.na(dat[f])),]
  }
  # automatically renumber the rows - we don't care about the internal R numbering as we have other unique identifiers per rwo
  row.names(dat) <- NULL
  droplevels(dat)
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
#' @param ifConflict - determines whether prefix or suffix is added to
#'   disambiguate conflicting column names. Value can be "suffix", "prefix".
#'   Suffix is the default.
#' @param doRename - regardless of column name clashes, "prefix" or "suffix"
#'   with every field with original table name, or "no" for neither
#' @param convertFactors Default is TRUE which causes factors to be converted to
#'   character before merge. This is almost certainly safer.
#'   @param verbose logical
#' @return merged data frame
#' @export
mergeBetter <- function(x, y, by.x, by.y,
                        all.x = FALSE, all.y = FALSE,
                        affix = NULL,
                        ifConflict = c("suffix", "prefix"),
                        doRename = c("no", "suffix", "prefix"),
                        convertFactors = TRUE,
                        verbose = FALSE) {

  ifConflict <- match.arg(ifConflict)
  doRename <- match.arg(doRename)

  # we don't want case sensitive names: we rely on case insensitivity, and it is
  # very common for the same data to have case-changes in the field name.
  stopifnot(all(!duplicated(tolower(names(x)))))
  stopifnot(all(!duplicated(tolower(names(y)))))

  # guess a good affix. If y is not just a variable name, use 'y'
  affix <- deparse(substitute(y))
  #if (any(grepl(pattern = "\\(|\\[", affix))) affix = "y"
  if (length(substitute(y)) > 1) affix = "y"

  if (class(x) != class(y)) warning(
    sprintf("x & y are different classes.
            They will be cast implicitly by the merge.
            Classes are: %s and %s", class(x), class(y)))

  # convert factors of keys only # TODO: as.integer may be appropriate sometimes/often.
  #TODO: tests for this
  if (convertFactors) {
    if (class(x[[by.x]]) == "factor") x[[by.x]] <- as.character(levels(x[[by.x]])[x[[by.x]]])
    if (class(y[[by.y]]) == "factor") y[[by.y]] <- as.character(levels(y[[by.y]])[y[[by.y]]])
  }

  if (verbose) {
    # this informational step could itself be slow in a big merge
    rightMergeDrops <- sum(!(x[[by.x]] %in% y[[by.y]]))
    leftMergeDrops <- sum(!(y[[by.y]]) %in% x[[by.x]])
    if (leftMergeDrops > 0 || rightMergeDrops > 0) {
      message(sprintf("mergeBetter: would drop %d out of %d from the new table", leftMergeDrops, nrow(y)))
      message(sprintf("and %d out of %d from the existing data", rightMergeDrops, nrow(x)))
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

  if (length(dupeNames_x) > 0) {
    if (doRename == "no") {
      if (verbose) message("there are conflicting field names in the merge but no renaming was specifically requested so using ifCOnflict to guide the renaming of clashing fields in x:",
                           paste(dupeNames_x, collapse = ", "))
      dropFields=c()
      for (xdup in dupeNames_x) {
        #rematch y - this is unsatisfying but simplifies the logic.
        match_x_in_y <- match(xdup, names(y))
        stopifnot(length(match_x_in_y) == 1)
        ydup <- names(y)[match_x_in_y]
        if (verbose) message("checking whether '", xdup, "' (matching '", ydup, "') has duplicated data.")
        isAllEqual <- all.equal(x[[xdup]], y[[ydup]])
        if (identical(isAllEqual, TRUE)) { # all.equal returns true or a char vector
          dropFields <- c(dropFields, ydup)
          if (verbose) message("will drop  identical field: ", ydup)
        } else {
          if (verbose) { message(isAllEqual); message("renaming conflicting field") }
          if (ifConflict == "suffix") {
            newName <- paste(ydup, affix, sep=".")
          } else if (ifConflict == "prefix") {
            newName <- paste(affix, ydup, sep=".")
          }
          names(y)[which(names(y) == ydup)] <- newName
        }
      }
      # actually drop the fields: best not to do while looping through the data frames.
      for (dropField in dropFields) y[dropField] <- NULL
    } else { # doRename = yes
      names(y) <- affixFields(fieldNames = names(y), skipFields = by.y,
                              affix = affix, doRename = doRename, verbose = verbose)
    }
  } # end if there are duplicates (no else - we can proceed)

  if (verbose) message(sprintf("merging using id field: %s, and new id field: %s", by.x, by.y))

  merge(x = x, by.x = by.x, all.x = all.x,
        y = y, by.y = by.y, all.y = all.y)
}

#' @title update a set of data frame field names
#' @description prefix or suffix
#' @param fieldNames char vector
#' @param skipFields char vector
#' @param affix character
#' @param doRename should be "suffix" or "prefix"
#' @param sep default '.'
#' @param verbose whether to display any informative messages
#' @return character vector, same length as fieldNames
#' @export
affixFields <- function(fieldNames, skipFields, affix,
                        doRename = c("no", "suffix", "prefix"),
                        sep = ".", verbose = FALSE) {

  doRename <- match.arg(doRename)

  if (doRename == "suffix") {
    if (verbose) message("renaming first table field names with suffix")
    fieldNames[fieldNames %nin% skipFields] <- paste(fieldNames[fieldNames %nin% skipFields], affix, sep = sep)
  } else if (doRename == "prefix") {
    if (verbose) message("renaming first table field names with prefix")
    fieldNames[fieldNames %nin% skipFields] <- paste(affix, fieldNames[fieldNames %nin% skipFields], sep = sep)
  } else {
    if (verbose) message(name="doRename = ", doRename, " so not adding prefix or suffix, but final merge might do so.")
  }
  fieldNames
}

#' @title get numbers that would be dropped in a merge
#' @description converts both vectors to numeric. This simulates merging when
#'   one key is character (but contains integer numbers), and another key is
#'   stored as integer.
#' @param x vector or factor
#' @param y vector or factor
#' @return list of two vectors
#' @export
getDroppedNumeric <- function(x, y) {
  x <- asNumericNoWarn(x)
  x <- asNumericNoWarn(y)
  list(
    missing_from_x = y[y %nin% x],
    missing_from_y = x[x %nin% y])
}

#' @title drop duplicate fields
#' @description compares all data in each field to every other field, and drops
#'   the latter match. Will find multiple matches. Doesn't do any type
#'   conversions yet. This is purely by content, not by field name.
#' @param df data.frame
#' @template verbose
#' @return data frame without duplicate fields
#' @export
dropDuplicateFields <- function(df, verbose = FALSE) {
  stopifnot(class(verbose) == "logical" && length(verbose) ==1)
  dropNames <- c()

  # to hell with vectorization
  for (f in 1:(dim(df)[2] - 1)) {
    for (g in (f + 1):dim(df)[2]) {
      if (identical(all.equal(df[[f]], df[[g]]), TRUE)) {
        if (verbose) message(sprintf("found matching fields %s and %s. Dropping the latter.",
                                     names(df)[f], names(df)[g]))
        dropNames <- c(dropNames, names(df)[g])
      }
    }
  }

  for (dn in dropNames) df[dn] <- NULL
  df
}
