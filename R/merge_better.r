#' Merge better
#'
#' Apply built-in R \code{\link[base]{merge}} but with additional features for
#' safety and information.
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
merge_better <- function(x, y, by.x, by.y,
                        all.x = FALSE, all.y = FALSE,
                        affix = NULL,
                        renameConflict = c("suffix", "prefix"),
                        renameAll = c("no", "suffix", "prefix"),
                        convert_factors = TRUE,
                        verbose = FALSE) {
  stopifnot(is.data.frame(x), is.data.frame(y))
  stopifnot(is.character(by.x), length(by.x) == 1L)
  stopifnot(is.character(by.y), length(by.y) == 1L)
  stopifnot(is.logical(all.x), length(all.x) == 1L)
  stopifnot(is.logical(all.y), length(all.y) == 1L)
  stopifnot(is.logical(convert_factors), length(convert_factors) == 1L)
  stopifnot(is.logical(verbose), length(verbose) == 1L)
  renameConflict <- match.arg(renameConflict)
  renameAll <- match.arg(renameAll)
  verbose <- as.integer(verbose) # TRUE will become low verbosity
  # we don't want case sensitive names: we rely on case insensitivity, and it is
  # very common for the same data to have case-changes in the field name.
  stopifnot(all(!duplicated(tolower(names(x)))))
  stopifnot(all(!duplicated(tolower(names(y)))))
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  # guess a good affix: `y` might be an expression and not suitable
  if (is.null(affix))
    affix <- make.names(y_name)
  # convert factors of keys only # TODO: as.integer may be appropriate
  # sometimes/often. TODO: tests for this. Ultimately may be better to
  # use data.table, and just index these columns properly.
  if (convert_factors)
    for (by_col in c(by.x, by.y))
      if (is.factor(x[[by_col]])) x[[by_col]] <- as_char_no_warn(x[[by_col]])
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
  if (anyDuplicated(x[[by.x]]) &&
      anyDuplicated(y[[by.y]]))
    warning("duplicate keys in both x and y will result in Cartesian",
    " expansion of the duplicates")
  m <- merge(x = x, by.x = by.x, all.x = all.x,
             y = y, by.y = by.y, all.y = all.y)
  stopifnot(anyDuplicated(names(m)) == 0)
  m
}

#' @rdname merge_better
#' @export
mergeBetter <- merge_better
