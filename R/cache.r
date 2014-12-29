#' @title option name for cache
#' @description allow use of \code{options} to search for the cache directory.
#' @family cache
#' @keywords character internal
optName <- "cachedir"

#' @title check whether an object is in the cache
#' @description use the same search algorithm as loading from the cache, but
#'   only report whether the file was there.
#' @template var
#' @template to_from_dates
#' @template cacheDir
#' @return logical, single logical value.
#' @import magrittr
#' @family cache
#' @return logical TRUE if found in cache
#' @export
isCached <- function(var,
                     from = NULL, to = NULL,
                     cacheDir = NULL) {
  stopifnot(length(var) == 1)
  stopifnot(length(cacheDir) == 1 || is.null(cacheDir))
  stopifnot(is.character(var))
  stopifnot(is.null(cacheDir) || is.character(cacheDir))
  stopifnot(is.null(from) || length(from) == 1 && is.Date(from))
  stopifnot(is.null(to) || length(to) == 1 && is.Date(to))
  stopifnot(!xor(is.null(from), is.null(to)))

  vn <- getCacheVarDated(var, from, to)

  # if from is specified, we can't rely on the (un-dated) var name being
  # correct, so we should actually look in the cache
  if (is.null(from)) {
    if (var %>% findCacheFilePath(cacheDir) %>% file.exists) return(TRUE)
  } else {
    if (getCacheVarDated(var, from = from, to = to) %>%
          findCacheFilePath(cacheDir) %>% file.exists) return(TRUE)
  }

  FALSE
}

#' @title Cache an R object
#' @description There are various memoise and cache functions in R but none did
#'   what I wanted. These functions allow a package to cache data in a standard
#'   place, or specified directory.
#'
#'   Calling environment is used by default to save
#' @template var
#' @template to_from_dates
#' @template cacheDir
#' @param envir environment to start searching for the cached data (it may
#'   already be in memory). Starts off with \code{parent.frame()} by default,
#'   and /code{inherits}, so should find already loaded cache files in
#'   .GlobalEnv eventually.
#' @template verbose
#' @export
#' @family cache
#' @return value in variable named by \code{var}
saveToCache <- function(var, from = NULL, to = NULL,
                        cacheDir = NULL, envir = parent.frame(),
                        verbose = FALSE) {
  stopifnot(is.null(from) || length(from) == 1 && is.Date(from))
  stopifnot(is.null(to) || length(to) == 1 && is.Date(to))
  stopifnot(!xor(is.null(from), is.null(to)))

  vn <- getCacheVarDated(var, from = from, to = to)

  if (verbose) message(sprintf(
    "saving '%s' to cache using name '%s' in cache dir %s",
    var, vn, cacheDir))

  save(list = var,
       envir = envir,
       file = findCacheFilePath(vn, cacheDir),
       compress = "xz")
  invisible(get(var, envir = envir))
}

#' @title get or load a variable from cache
#' @description Get data from the cache, optionally with date ranges. load from
#'   cache also returns the variable, but additionally saves the data by its
#'   undated cache name in the given environment, defaulting to the calling
#'   frame.
#' @template var
#' @template to_from_dates
#' @template cacheDir
#' @template verbose
#' @export
getFromCache <- function(var,
                         from = NULL, to = NULL,
                         cacheDir = NULL, verbose = FALSE) {
  stopifnot(is.null(from) || length(from) == 1 && is.Date(from))
  stopifnot(is.null(to) || length(to) == 1 && is.Date(to))
  stopifnot(!xor(is.null(from), is.null(to)))
  vn <- getCacheVarDated(var, from = from, to = to)

  fp <- findCacheFilePath(vn, cacheDir)
  if (!file.exists(fp))
    stop(sprintf("'%s' doesn't exist when trying to access cache", fp))

  vl <- load(file =  fp)
  if (verbose) message(sprintf(
    "loaded '%s' from cache from var '%s' and dated name '%s'",
    vl, var, vn))
  # we are assuming that the .RData file contains a variable with the same name
  # as the file name (minus the file extension). if the data has a date range,
  # then we use the undated variable name. Double check this now.
  stopifnot(length(vl) == 1)
  stopifnot(var == vl)
  get(vl)
}

#' @rdname getFromCache
#' @param ... params to pass on to getFromCache
#' @param envir environment in which to load the variable, defaults to the
#'   calling frame.
#' @param inherits single logical, passed on to \code{assign}
#' @export
loadFromCache <- function(var, ..., envir = parent.frame(), inherits = FALSE) {
  assign(var, getFromCache(var, ...),
         envir = envir, inherits = inherits)
  invisible(get(var, envir = envir, inherits = inherits))
}

#' @rdname cache
#' @export
assignCache <- function(value, var,
                        from = NULL, to = NULL, ...,
                        envir = parent.frame(),
                        verbose = FALSE)
  cache(value = value, var = var, from = from, to = to, ...,
        envir = envir, assign = TRUE, verbose = verbose)

#' @title cache and assign data to an environment
#' @description Same as assign, but will only touch cache if the variable isn't
#'   already loaded.
#' @param value value to be lazy evaluated. Consider also enabling a call, or
#'   expression. \code{eval} could be called on it, so a constructed expression
#'   can be passed in. Since scoping changes, this should be self contained.
#' @template var
#' @template to_from_dates
#' @template cacheDir
#' @param envir environment to assign to, defaults to the calling frame
#' @param force single logical value whether to force re-writing of cache if
#'   data already exists
#' @param assign single logical value whether to assign the cached data to an
#'   environment
#' @param format single character value with date format
#' @param ... arguments passed on to other functions
#' @param fun function which accepts from and to arguments used by
#'   \code{assignCacheByFun}
#' @template verbose
#' @return invisibly returns the value assigned
#' @family cache
#' @export
cache <- function(value, var,
                  from = NULL, to = NULL,
                  cacheDir = NULL,
                  envir = parent.frame(),
                  force = FALSE,
                  assign = FALSE,
                  format = "%Y-%m-%d",
                  verbose = FALSE) {

  stopifnot(length(var) == 1)
  stopifnot(is.character(var))
  stopifnot(length(from) <= 1)
  stopifnot(length(to) <= 1)
  stopifnot(is.null(from) || length(from) == 1 && is.Date(from))
  stopifnot(is.null(to) || length(to) == 1 && is.Date(to))
  stopifnot(!xor(is.null(from), is.null(to)))
  stopifnot(is.null(cacheDir) || is.character(cacheDir) && file.exists(cacheDir))
  stopifnot(is.environment(envir))
  stopifnot(is.logical(force))
  stopifnot(is.character(format))
  stopifnot(length(format) == 1)

  # "value" should not be evaluated until used, so a database query in 'value'
  # should be ignored if not needed, and not throw an error if database not
  # available.
  if (!force && isCached(var = var,
                         from = from, to = to,
                         cacheDir = cacheDir)) {
    if (verbose) message(sprintf("'%s' already cached and not forcing.", var))
    if (assign) assign(var, getFromCache(var = var, from = from, to = to,
                                         cacheDir = cacheDir, verbose = verbose),
                       envir = envir)
    return(invisible(get(var, envir = envir)))
  }

  if (verbose) message(sprintf("'%s' with dates %s and %s not in cache or forced",
                               var, from, to))
  # saveToCache returns the value it saved, so we just return the result here.
  if (!assign) envir = environment()
  assign(x = var, value = value, envir = envir)
  saveToCache(var = var,
              from = from, to = to,
              cacheDir = cacheDir, envir = envir)
}

#' @rdname cache
#' @export
assignCacheByFun <- function(fun, var,
                             from = NULL, to = NULL, ...,
                             cacheDir = NULL,
                             envir = parent.frame())
  assignCache(value = fun(from = from, to = to, ...),
              var = var,
              from, to,
              cacheDir = cacheDir,
              envir = envir)


#' @title find path to the cache directory
#' @description Searches a few likely places: - directory specified in
#'   \code{cacheDir} - session-wide R option "cacheDir" - \code{cache} directory
#'   within the working directory - \code{cache} directory within the parent
#'   directory Fail with error if we have still not found it. Walking up the
#'   directory tree is both time consuming, low yield, and unreliable, so this
#'   is no longer done. Instead, a fall-back cache directory is used based on an
#'   /code{options} setting jwutil.fallbackCacheDir, which defaults to ~/jwutil.
#'   Would prefer to use /tmp but this doesn't exist on Windows, and using the R
#'   built-in to generate a temporary directory is not possible with a static
#'   option. Generating it on-the-fly would need passing the cache dir around
#'   between functions or creating a singleton, all of which get too complicated
#'   for this corner case. Of note, R CMD check creates a distinct directory
#'   tree with all the testing files, and may not include the \code{cache}
#'   directory from the working tree, e.g. if the cache directory is in
#'   \code{.Rbuildignore}.
#' @template cacheDir
#' @param cacheDirName single character string, defaults to 'jwcache'. 'cache'
#'   alone is not distinctive, and conflicts with other things, such as the
#'   cache directory in the vignettes directory.
#' @template verbose
#' @import magrittr
#' @family cache
#' @export
findCacheDir <- function(cacheDir = NULL, cacheDirName = "jwcache",
                         verbose = FALSE) {
  if (!is.null(cacheDir) && file.exists(cacheDir)) return(cacheDir)
  if (!is.null(getOption(optName)) && file.exists(getOption(optName)))
    return(getOption(optName))
  td <- file.path(getwd(), cacheDirName)
  if (file.exists(td)) return(td)

  # parents: this is good when stuck e.g. in vignette sub-directory of a project
  td <- getwd() %>% dirname %>% file.path(cacheDirName)
  if (file.exists(td)) return(td)
  td <- getwd() %>% dirname %>% dirname %>% file.path(cacheDirName)
  if (file.exists(td)) return(td)
  td <- getwd() %>% dirname %>% dirname %>% dirname %>% file.path(cacheDirName)
  if (file.exists(td)) return(td)
  # parent of parent of parent of parent (believe it or not, this has use cases)
  td <- getwd() %>% dirname %>% dirname %>% dirname %>% dirname %>%
    file.path(cacheDirName)
  if (file.exists(td)) return(td)

  if (verbose) message("No cache directory within directory: ", getwd())
  if (verbose && platformIsLinux())
    system(sprintf("locate --regex  %s$", cacheDirName), intern = TRUE) %>%
    paste(sep=", ", collapse=", ") %>% message
  if (verbose) message("Use the cacheDir= argument to specify it directly,
          or check the cache was created in the correct place")
  fb <- options("jwutil.fallbackCacheDir")
  if (verbose) message("using fallback cache directory: ")
  td <- getOption("jwutil.fallbackCacheDir")
  if (!file.exists(td)) dir.create(td)
  td
}

#' @title find path to a file in cache directory
#' @param filename character vector of file names
#' @template cacheDir
#' @family cache
#' @export
findCacheFilePath <- function(filename, cacheDir = NULL) {
  cacheDir <- findCacheDir(cacheDir)
  if (!file.exists(cacheDir))
    stop("could not find cache directory: ", cacheDir)
  file.path(cacheDir, paste0(filename, ".RData"))
}

#' @title save data compressed in data folder
#' @description xz appears to fail on Windows, so use bzip2
#'
#'   tools::checkRdaFiles(file.path("data", list.files(path = "data")))
#'   tools::resaveRdaFiles(file.path("data", list.files(path = "data")),
#'   compress = "xz")
#' @template var
#' @param suffix char additional characters before ".RData"
#' @family cache
#' @export
saveInDataDir <- function(var, suffix) {
  save(list = var,
       envir = parent.frame(),
       file = file.path('data', strip(paste0(var, suffix, '.RData'))),
       compress = ifelse(platformIsWindows(), "bzip2", "xz")
  )
}

#' @title remove from cache and given environment
#' @description TOOD: warn if not found
#' @template var
#' @param pattern character regular expression
#' @template to_from_dates
#' @template cacheDir
#' @family cache
#' @export
rmCache <- function(var = NULL, pattern = NULL,
                    from = NULL, to = NULL,
                    cacheDir = NULL) {
  stopifnot(is.null(from) || length(from) == 1 && is.Date(from))
  stopifnot(is.null(to) || length(to) == 1 && is.Date(to))
  stopifnot(!xor(is.null(from), is.null(to)))
  stopifnot(xor(is.null(var), is.null(pattern)))

  if (!is.null(var)) {
    vn <- getCacheVarDated(var, from = from, to = to)

    if (isCached(var = var, from = from, to = to, cacheDir = cacheDir))
      file.remove(findCacheFilePath(vn, cacheDir))

  } else
    file.remove(lsCacheFiles(cacheDir = cacheDir, pattern = pattern))

}

#' @title list cache contents
#' @param pattern regex, ".Rdata$" is appended within the function for file name
#'   searches
#' @template cacheDir
#' @family cache
#' @export
lsCacheFiles <- function(pattern = ".*", cacheDir = NULL)
  list.files(path = findCacheDir(cacheDir = cacheDir),
             pattern = paste0(pattern, "\\.RData$"))

#' @rdname lsCacheFiles
#' @export
lsCache <- function(pattern = ".*", cacheDir = NULL)
  sub(pattern = "\\.RData", replacement = "", lsCacheFiles(pattern, cacheDir))

#' @title rename cache file(s)
#' @description renames a cache file, or set of dated files. It has the ability
#'   to rename a single dated file, if the dated name is given, but probably
#'   best to keep all the files for the same root name in sync. The trick is not
#'   just renaming the files, but renaming the variable names of the data
#'   therein.
#' @param oldname character vector length one
#' @param newname character vector length one
#' @param cacheDir character vector length one
#' @template verbose
#' @family cache
#' @export
renameCache <- function(oldname, newname, cacheDir = NULL,
                        verbose = FALSE) {

  stopifnot(is.null(cacheDir) || length(cacheDir) == 1)
  ondisk <- lsCache(pattern = paste0("^", oldname, ".*"), cacheDir = cacheDir)
  stopifnot(length(ondisk) > 0)

  # for each cached file, load into work env, assign data to new var name
  for (vo in ondisk) {
    vn <- sub(pattern = oldname, replacement = newname, x = vo)
    if (verbose) message(sprintf("renaming '%s' to '%s'", vo, vn))
    file.rename(findCacheFilePath(vo, cacheDir),
                findCacheFilePath(vn, cacheDir))
  }
}

#' @title make cache file name from dates and unadorned variable name
#' @template var
#' @template to_from_dates
#' @family cache
#' @export
getCacheVarDated <- function(var, from = NULL, to = NULL) {
  stopifnot(is.null(from) || length(from) == 1 && is.Date(from))
  stopifnot(is.null(to) || length(to) == 1 && is.Date(to))
  stopifnot(!xor(is.null(from), is.null(to)))
  stopifnot(length(var) == 1)
  paste(var,
        paste(from, to, sep = "to"),
        sep = "")
}
