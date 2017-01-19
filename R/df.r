#' @title encode TRUE as 1, and FALSE as 0 (integers)
#' @description when saving data as text files for distribution, printing large
#'   amounts of text containing TRUE and FALSE is inefficient. Convert to binary
#'   takes more R memory, but allows more compact output TODO: test
#' @param x dataframe which may contain logical fields
#' @return data frame without logical fields
#' @keywords manip
#' @export
logicalToBinary <- function(x) {
  stopifnot(is.data.frame(x))
  if (any(dim(x) == 0))
    stop("got zero in at least one dimension in data frame. %d, %d",
         dim(x)[1], dim(x)[2])

  # can condense this code into a one-liner, but this is clearer:
  logical_fields <- names(x)[sapply(x, is.logical)]
  if (is.na(logical_fields) || length(logical_fields) == 0) return(x)

  #update just the logical fields with integers
  x[, logical_fields] <-
    vapply(
      X         = x[, logical_fields],
      FUN       = function(y) ifelse(y, 1L, 0L),
      FUN.VALUE = integer(length = dim(x)[1])
    )
  x
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
  checkmate::assertFactor(fctr)
  checkmate::assertString(prefix)
  checkmate::assertString(sep)
  checkmate::assertFlag(na.rm)
  checkmate::assertFlag(verbose)
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

  if (verbose)
    message("more than two factor levels")
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
#' @param x data.frame to search for factors to convert
#' @param consider character vector of field names in the data frame to
#'   consider. Defaults to all fields
#' @param sep character scalar used to separate field prefixes from factor
#'   values in new column names
#' @param na.rm logical scalar: if NA data and/or NA levels, then covert to NA
#'   strings and expand these as for any other factor
#' @template verbose
#' @return data.frame with no factors
#' @export
#' @seealso \code{PSAgraphics::cv.trans.psa}
expandFactors <- function(x,
                          consider = names(x),
                          sep = "",
                          na.rm = TRUE,
                          verbose = FALSE) {
  if (verbose)
    message("converting factors in data frame into logical vectors")

  # identify which of the last of fields is actually a factor
  factor_names <- getFactorNames(x, consider)

  if (length(factor_names) > 0) {
    if (verbose) message("there are factors to be converted into values: ",
                         paste(factor_names, collapse = ", "))
    for (fn in factor_names) {
      if (verbose) message(sprintf("working on factor: %s", fn))
      dfpart <- factorToDataframeLogical(
        fctr = x[[fn]], prefix = fn, sep = sep,
        na.rm = na.rm, verbose = verbose)
      x[fn] <- NULL
      x <- cbind(x, dfpart)
    }
  } else {
    if (verbose) message("no factors found to convert in exFactor")
  }
  x
}

#' @title get names of the factor fields in a data frame
#' @description Get the names of those fields in a data frame which are factors.
#' @param x data frame
#' @param consider character vector of field names of the data frame to test,
#'   default is to use all of them.
#' @return vector
#' @export
getFactorNames <- function(x, consider = names(x)) {
  if (length(names(x)) <= 0 || length(consider) <= 0)
    return()

  consider[sapply(x[1, consider], is.factor)]
  #if (anyDuplicated) #TODO
}

#' @rdname getFactorNames
#' @export
getNonFactorNames <- function(x, consider = names(x)) {
  consider[consider %nin% getFactorNames(x, consider)]
}

#' @title get NA field names from data frame
#' @description Get the names of any columns in a data frame which have NA
#'   values.
#' @param dframe data.frame
#' @return vector of names of fields which contain any NA values, length zero if
#'   no matches
#' @export
getNAFields <- function(dframe) {
  stopifnot(is.data.frame(dframe))
  naFields <- names(dframe)[sapply(dframe, countIsNa) > 0]
  if (length(naFields) == 0)
    character()
  else
    naFields
}

#' @rdname getNAFields
#' @export
getNonNAFields <- function(dframe)
  names(dframe)[names(dframe) %nin% getNAFields(dframe)]

#' @title return proportion of NA values per field
#' @description Return proportion of values which are \code{NA} in each field of
#'   the given data frame.
#' @param dframe is a data frame
#' @return numeric vector
#' @export
propNaPerField <- function(dframe)
  sapply(dframe, function(v) {
    countIsNa(v) / length(v)
  })

#' @title drops rows with NA values in specified fields
#' @description employs \code{stats::complete.cases} which is fast internal C
#'   code. Returns a data frame with unused factor levels dropped (these may
#'   have been introduced by dropping rows with some NA values)
#' @param x data frame
#' @param fld vector with names of fields which must have no NA values
#' @template verbose
#' @return data frame without rows containing NA in the specified data fields.
#'   There may be NA values in the resulting data frame in fields which are not
#'   listed in fld.
dropRowsWithNAField <- function(x, fld = names(x), verbose = FALSE) {
  if (verbose)
    message(sprintf("checking fields: %s for NA values",
                    paste(fld, sep = ", ")))
  stopifnot(is.character(fld))
  stopifnot(is.data.frame(x))
  cc <- stats::complete.cases(x[fld])
  droplevels(x[cc, ])
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
#' @param convert_factors Default is TRUE which causes factors to be converted
#'   to character before merge. This is almost certainly safer.
#' @param verbose logical or numbers 0, 1 or 2. 1 or TRUE will give moderate
#'   verbosity, 2 will give full verbosity. 0 or FALSE turns off all messages.
#' @return merged data frame
#' @examples
#' df <- data.frame(a= c("1","2"), b = 1:2, stringsAsFactors = FALSE)
#' eg <- data.frame(a= c("1","3"), b = 3:4, stringsAsFactors = FALSE)
#' mergeBetter(x = df, y = eg, by.x = "a", by.y = "a", verbose = TRUE)
#' @export
mergeBetter <- function(x, y, by.x, by.y,
                        all.x = FALSE, all.y = FALSE,
                        affix = NULL,
                        renameConflict = c("suffix", "prefix"),
                        renameAll = c("no", "suffix", "prefix"),
                        convert_factors = TRUE,
                        verbose = FALSE) {

  checkmate::assertDataFrame(x, )
  checkmate::assertDataFrame(y)
  checkmate::assertCharacter(by.x, min.len = 1, min.chars = 1)
  checkmate::assertCharacter(by.y, min.len = 1, min.chars = 1)
  checkmate::assertFlag(all.x)
  checkmate::assertFlag(all.y)
  renameConflict <- match.arg(renameConflict)
  renameAll <- match.arg(renameAll)
  checkmate::assertFlag(convert_factors)
  checkmate::assert(checkmate::checkFlag(verbose), checkmate::checkInt(verbose))

  verbose <- as.integer(verbose) # TRUE will become low verbosity

  # we don't want case sensitive names: we rely on case insensitivity, and it is
  # very common for the same data to have case-changes in the field name.
  stopifnot(all(!duplicated(tolower(names(x)))))
  stopifnot(all(!duplicated(tolower(names(y)))))

  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  # guess a good affix: `y` might be an expression and not suitable
  if (is.null(affix)) {
    affix <- make.names(y_name)
    #if (length(substitute(y)) > 1) affix <- "y"
  }

  # convert factors of keys only # TODO: as.integer may be appropriate
  # sometimes/often. TODO: tests for this. Ultimately may be better to
  # use data.table, and just index these columns properly.
  if (convert_factors) {
    for (by_col in c(by.x, by.y)) {
      if (is.factor(x[[by_col]]))
        x[[by_col]] <- asCharacterNoWarn(x[[by_col]])
    }
  }

  if (verbose) {
    # this would be more efficient with data table or sql type query
    comb_key_x <- apply(x[by.x], 1, paste, collapse = "j")
    comb_key_y <- apply(y[by.y], 1, paste, collapse = "j")
    left_missing <- sum(comb_key_x %nin% comb_key_y)
    right_missing  <- sum(comb_key_y %nin% comb_key_x)
    if (right_missing + left_missing > 0) {
      message(sprintf(ifelse(all.y,
                             "keeping %d out of %d unmatched from %s",
                             "dropping %d out of %d from %s"
      ), right_missing, nrow(y), y_name, y_name))
      message(sprintf(ifelse(all.x,
                             "keeping %d out of %d unmatched from %s",
                             "dropping %d out of %d from %s"
      ), left_missing, nrow(x), x_name, x_name))
    } else
      message("Keys match exactly, so dropping no rows.")
  }

  # find duplicate field names, ignoring the field we are merging on.
  dupes_x <- names(x)[tolower(names(x)) %in% tolower(names(y)) &
                        tolower(names(x)) %nin% tolower(by.x) &
                        tolower(names(x)) %nin% tolower(by.y)]
  # drop identical fields unless an explicit rename has been requested.
  if (length(dupes_x) > 0 && renameAll == "no") {
    if (verbose)
      message(sprintf("%s field names duplicated in %s: %s", x_name, y_name,
              paste(dupes_x, collapse = ", ")))

    if (verbose > 1)
      message(sprintf("Adding %s to rename conflicts in %s: %s", y_name,
                      renameConflict))
    dropFields <- c()
    for (xdup in dupes_x) {
      #rematch y - this is unsatisfying but simplifies the logic.
      match_x_in_y <- match(tolower(xdup), tolower(names(y)))
      stopifnot(length(match_x_in_y) == 1)  # two conflicts with that name!
      ydup <- names(y)[match_x_in_y]
      if (verbose > 1) message("checking whether '", xdup,
                               "' (matching '", ydup, "') has duplicated data.")
      isAllEqual <- all.equal(x[[xdup]], y[[ydup]])

      # all.equal returns true or a char vector, so work around
      if (identical(isAllEqual, TRUE)) {
        if (verbose > 1) message("dropping identical field: ", ydup)
        dropFields <- c(dropFields, ydup)
      } else {
        if (verbose > 1) message("renaming non-identical field:", ydup)
        names(y)[which(names(y) == ydup)] <-
          affixFields(fields = ydup,
                      affix = affix,
                      renameHow = renameConflict)
      }
    }
    # drop the fields: best not to do while looping through the data frames.
    for (dropField in dropFields) y[dropField] <- NULL
  } else if (renameAll != "no") {
    names(y) <- affixFields(fields = names(y), skip = by.y,
                            affix = affix, renameHow = renameAll)
  }

  if (verbose) message(sprintf("merging using id: %s, and new id: %s\n",
                               by.x, by.y))

  m <- merge(x = x, by.x = by.x, all.x = all.x,
             y = y, by.y = by.y, all.y = all.y)

  stopifnot(anyDuplicated(names(m)) == 0)
  m
}

#' @title update a set of data frame field names
#' @description prefix or suffix
#' @param fields char vector
#' @param affix character
#' @param skip char vector, defaults to include all fields
#' @param renameHow should be "suffix" or "prefix", default is suffix
#' @param sep default "."
#' @return character vector, same length as fields
#' @export
affixFields <- function(fields, affix, skip = NULL,
                        renameHow = c("suffix", "prefix"),
                        sep = ".") {

  stopifnot(length(affix) == 1)
  stopifnot(nchar(affix) > 0)
  stopifnot(is.null(skip) || is.character(skip))

  renameHow <- match.arg(renameHow)
  if (renameHow == "suffix") {
    fields[fields %nin% skip] <-
      paste(fields[fields %nin% skip], affix, sep = sep)
  } else {
    fields[fields %nin% skip] <-
      paste(affix, fields[fields %nin% skip], sep = sep)
  }
  fields
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
  stopifnot(is.logical(verbose), length(verbose) == 1)
  drop <- c()

  # to hell with vectorization
  for (f in 1:(dim(df)[2] - 1)) {
    for (g in (f + 1):dim(df)[2]) {
      if (identical(all.equal(df[[f]], df[[g]]), TRUE)) {
        if (verbose) message(sprintf("found matching fields %s and %s. \
                                     Dropping one.",
                                     names(df)[f], names(df)[g]))
        drop <- c(drop, names(df)[g])
      }
    }
  }

  for (dn in drop) df[dn] <- NULL
  df
}

#' @title filter data with diagnostics
#' @description applies an expression to a data frame, and gives information
#'   about the numbers of dropped rows.
#' @param x data frame
#' @param expr expression in the context of the data frame, i.e. the terms
#'   should be column names.
#' @param verbose logical default is TRUE
#' @export
#' @return filtered data frame
filterBetter <- function(x, expr, verbose = TRUE) {
  sexpr <- substitute(expr)
  fn <- deparse(substitute(x))
  if (fn == ".") fn <- "" else fn <- paste("'", fn, "'")

  fltr <- eval(sexpr, x)
  stopifnot(is.logical(fltr))

  if (verbose) message(sprintf(
    "Filtering %s with expression '%s' drops %d (%2.1f%%) of %d",
    fn, deparse(sexpr),
    sum(!fltr), 100 * sum(!fltr) / length(fltr), length(fltr)
  ))
  x[fltr, ]
}

#' @title names of fields which are numeric, binary or combinations thereof
#' @description Doesn't make any allowance for factors.
#' @param x data frame
#' @param invert single logical, if true, will return non-binary columns
#' @return vector of column names
#' @examples
#' dat <- data.frame(c("a", "b"), c(TRUE, FALSE), c(1, 0), c(1L, 0L),
#'                   c(1L, 2L), c(0.1, 0.2), c("9", "8"))
#' names(dat) <- c("char", "bin", "binfloat", "binint",
#'                 "int", "float", "charint")
#' binary_cols(dat)
#' binary_col_names(dat)
#' binary_col_names(dat, invert = TRUE)
#' @export
binary_col_names <- function(x, invert = FALSE) {
  checkmate::assertDataFrame(x)
  checkmate::assertFlag(invert)
  names(x)[xor(sapply(x, function(y) all(y %in% c(0, 1))), invert)]
}

#' @rdname binary_col_names
#' @export
binary_cols <- function(x, invert = FALSE) {
  checkmate::assertDataFrame(x)
  checkmate::assertFlag(invert)
  x[binary_col_names(x = x, invert = invert)]
}

#' @describeIn binary_col_names Find character columns which are really numeric
#' @param attrition If less than this proportion of rows become \code{NA} on
#'   conversion to numeric, then accept this is a numeric column after all.
#' @export
numeric_char_col_names <- function(x, invert = FALSE, attrition = 0.05) {
  checkmate::assertDataFrame(x)
  checkmate::assertFlag(invert)

  char_cols <- sapply(x, is.character)
  was_na <- colSums(is.na(x[char_cols]))
  numberish <- colSums(is.na(asNumericNoWarn(x[char_cols])))
  new_na_ratio <- (numberish - was_na) / (nrow(x) - was_na)
  new_na_ratio
}

#' @rdname binary_col_names
#' @export
numeric_col_names <- function(x, invert = FALSE) {
  checkmate::assertDataFrame(x)
  checkmate::assertFlag(invert)

  names(x)[sapply(x, function(y) {
    z <- is.numeric(y)
    if (invert)
      !z
    else
      z
  })]
}

#' @rdname binary_col_names
#' @export
numeric_cols <- function(x, invert = FALSE) {
  checkmate::assertDataFrame(x)
  checkmate::assertFlag(invert)
  x[numeric_col_names(x = x, invert = invert)]
}

#' @title fill out missing combinations of factors with NA
#' @description fill out missing combinations of factors with NA
#' @param df data frame
#' @details Adapated from
#'   \url{http://www.cookbook-r.com/Manipulating_data/Summarizing_data/#using-aggregate}
#' @export
fillMissingCombs <- function(df) {
  levelList <- list()
  for (f in getFactorNames(df)) levelList[[f]] <- levels(df[, f])
  merge(expand.grid(levelList), df, all.x = TRUE)
}

#' @title zero NA values in a data.frame
#' @description Zero NA values in a data.frame, including \code{cols} and
#'   exluding \code{ignore}. Also does not replace \code{Date} or \code{POSIXt}
#'   fields.
#' @param df data.frame
#' @param cols names of columns to work on, default is all columns
#' @param  ignore cahracter vector of columns names to ignore
#' @param verbose TRUE or FALSE
#' @export
zero_na <- function(df, cols = names(df), ignore = character(), verbose = FALSE) {
  checkmate::assertDataFrame(df)
  checkmate::assertCharacter(cols)
  checkmate::assertCharacter(ignore)
  checkmate::assertFlag(verbose)
  stopifnot(all(c(cols, ignore) %in% names(df)))
  gotNA <- getNAFields(df)
  for (n in gotNA[gotNA %nin% ignore]) {
    x <- df[[n]]
    if (verbose)
      message(sprintf("zeroing NA values in %s", n))
    if (!methods::is(x, "POSIXt") && !is.Date(x) && !is.factor(x)) {
      df[is.na(x), n] <- 0
    } else if (verbose)
      message(sprintf("skipping factor or Date: %s", n))
  }
  df
}

#' minimal basic pre-processing metrics
#' @param x data.frame input
#' @export
jw_df_basics <- function(x, df_list) {
  stopifnot(xor(missing(x), missing(df_list)))
  if (!missing(x))
    df_list <- list(x)

  checkmate::assertList(df_list, types = "data.frame", min.len = 1)

  out <- lapply(df_list, .jw_df_basics_impl)
  if (length(out) > 1)
    out
  else
    out[[1]]

}

.jw_df_basics_impl <- function(x) {
  checkmate::assertDataFrame(x)

  n <- nrow(x)
  cl = lapply(x, class)
  f <- vapply(x, is.factor, logical(1))
  u <- sapply(iris, function(y) length(unique(y)))
  n_na <- colSums(is.na(x))
  suppressWarnings({
    n_neg <- colSums(x < 0)
    n_zero <- colSums(x == 0)
  })

  n_neg[f] <- NA
  n_zero[f] <- NA

  cbind(
    name = names(x),
    class = cl,
    typeof = lapply(x, typeof),
    n_na, p_na = n_na / n,
    n_neg, p_neg = n_neg / n,
    n_zero, p_zero = n_zero / n,
    n_uniq = u,
    p_uniq = u / n
  )
}
