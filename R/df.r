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
#' @description converts a factor into a data.frame with multiple T/F fields,
#'   one for each factor
#' @param fctr factor
#' @param prefix defaults to "f" to pre-pend the factor level when constructing
#'   the data frame columns names
#' @return data.frame with columns of logicals
#' @export
factorToCols <- function(fctr, prefix = "f", verbose = FALSE) {

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

  if (verbose) message("looping to extract logical vectors from the provided factor")
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
expandFactors <- function (dframe, considerFactors = names(dframe), verbose = FALSE) {
  if (verbose) message("exFactor: converting factors in a data frame into logical vectors")

  #message("considerFactors: %s", paste(considerFactors, collapse=', '))

  # identify which of the last of fields is actually a factor
  factorNames = getFactorNames(dframe, considerFactors)

  #message("got factorNames: %s", paste(factorNames, collapse=", "))

  newCols <- vector()
  if (length(factorNames)>0) {
    if (verbose) message("there are factors to be converted into values", paste(factorNames, collapse=", "))
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
    if (verbose) message("no factors found to convert in exFactor")
  }
  if (length(newCols) == 0) newCols <- NULL

  if (verbose) message("newFields: %s", paste(newCols, collapse=", "))

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
#'   disambiguate conflicting column names. Value can be "suffix", "prefix". Suffix is the default.
#' @param doRename - regardless of column name clashes, "prefix" or "suffix"
#'   with every field with original table name, or "no" for neither
#'
#'   1. check whether matching name fields are identical, and if so, don't
#'   duplicate in merge output
#'
#'   2. more sophisticated renaming of conflicting fields
#'
#'   3. logging of the overlap between the data sets
#'
#'   4. limit just to left outer join
#' @return merged data frame
#' @export
mergeBetter <- function(x, y,
                        by.x, by.y,
                        all.x = FALSE, all.y = FALSE,
                        affix = deparse(substitute(y)),
                        ifConflict = c("suffix", "prefix"),
                        doRename = c("no", "suffix", "prefix"),
                        verbose = FALSE) {

  ifConflict <- match.arg(ifConflict)
  doRename <- match.arg(doRename)

  if (class(x) != class(y)) warning(
      sprintf("x & y are different classes.
              They will be cast implicitly by the merge.
              Classes are: %s and %s", class(x), class(y)))

  # this informational step could itself be slow in a big merge
  rightMergeDrops <- sum(!(x[[by.x]] %in% y[[by.y]]))
  leftMergeDrops <- sum(!(y[[by.y]]) %in% x[[by.x]])
  if (leftMergeDrops > 0 | rightMergeDrops > 0) {
    if (verbose) message(
      sprintf("mergeBetter: would drop %d out of %d from the new table, and %d out of %d from the existing data",
              leftMergeDrops, nrow(y), rightMergeDrops, nrow(x)
              )
    )
  } else {
    if (verbose) message("no rows will be dropped in the merge - keys match exactly.
            There may still be data differences in the two data frames.")
  }

  # find duplicate field names, ignoring the field we are merging on.
  duplicateFieldNames <- names(x)[ names(x) %in% names(y) & !names(x) == by.y]

  if (length(duplicateFieldNames) > 0 && doRename == "no") {
    if (verbose) message("there are conflicting field names in the merge but
            no prefix or suffix was requested: ", duplicateFieldNames)
    for (n in duplicateFieldNames) {
      if (identical(x[n], y[n])) {
        y[n] <- NULL # drop the field if it is identical to another one with the same name
        if (verbose) message("dropping identical field: ", n) # and warn?
      } else { # rename individual conflicting fields
        if (ifConflict == "suffix") {
          newName <- paste(n, affix, sep=".")
        } else if (ifConflict == "prefix") {
          newName <- paste(affix, n, sep=".")
        }
        names(y)[which(names(y) == n)] <- newName
      }
    }
  }

  names(y) <- affixFields(fieldNames = names(y),
                          skipFields = by.y,
                          affix = affix,
                          doRename = doRename,
                          # sep = default (".")
                          verbose = verbose)

  #sprintf(name="jh.mergeBetter", "merging table '%s' using merged id field: %s, and new id field: %s", t, by,x, by.y)
  merged <- merge(
    x = x,
    y = y,
    by.x = by.x,
    by.y = by.y,
    all.x = all.x,
    all.y = all.y,
    suffixes = c("", paste0(".", affix)) # if we didn't already rename, then allow merge itself to rename if conflict.
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
    if (verbose) message(name="not adding prefix or suffix to first table because doRename = ", doRename)
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
    missingFromTwo = one[one %nin% two])
}
