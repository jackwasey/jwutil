#' @title option name for cache
#' @description allow use of \code{options} to search for the cache directory.
#' @family cache
#' @keywords character internal
optName <- "cachedir"

#' @title check whether an object is in the cache
#' @description use the same search algorithm as loading from the cache, but
#'   only report whether the file was there.
#' @template varName
#' @param startDate yyyy-mm--dd
#' @param endDate yyyy-mm--dd
#' @template cacheDir
#' @param force single logical. If TRUE, then actually look in the cache
#'   directory, otherwise we are satisfied if the variable already exists in the
#'   given environment. This exactly parallels \code{loadFromCache}
#' @param envir environment in which to check whether the data is already loaded
#'   (force = TRUE will skip this test). Default is \code{parent.frame()}
#' @return logical, single logical value.
#' @family cache
#' @import magrittr
#' @export
isCached <- function(varName, cacheDir = NULL, startDate = NULL, endDate = NULL,
                     force = FALSE, envir = parent.frame()) {
  stopifnot(length(varName) == 1,
            length(cacheDir) == 1 || is.null(cacheDir),
            length(force) == 1)
  stopifnot(is.character(varName),
            is.logical(force),
            is.environment(envir))
  stopifnot(is.null(cacheDir) || is.character(cacheDir))
  stopifnot(is.null(startDate) ||
              length(startDate) == 1 && is.character(startDate))
  stopifnot(is.null(endDate) || length(endDate) == 1 && is.character(endDate))
  stopifnot(!xor(is.null(startDate), is.null(endDate)))

  if (!is.null(startDate))
    vn <- getCacheVarDated(varName, startDate, endDate)
  else
    vn <- varName

  # if startDate is specified, we can't rely on the (un-dated) var name being
  # correct, so we should actually look in the cache
  if (is.null(startDate)) {
    if (!force && exists(varName, envir = envir)) return(TRUE)
    if (varName %>% findCacheFilePath(cacheDir) %>% file.exists) return(TRUE)
  } else {
    if (getCacheVarDated(varName, startDate = startDate, endDate = endDate) %>%
          findCacheFilePath(cacheDir) %>% file.exists) return(TRUE)
  }

  FALSE
}

#' @title Cache an R object
#' @family cache
#' @description There are various memoise and cache functions in R but none did
#'   what I wanted. These functions allow a package to cache data in a standard
#'   place, or specified directory.
#'
#'   Parent environment is used by default to save
#' @template varName
#' @template startEndDate
#' @template cacheDir
#' @param envir environment to start searching for the cached data (it may
#'   already be in memory). Starts off with \code{parent.frame()} by default,
#'   and /code{inherits}, so should find already loaded cache files in
#'   .GlobalEnv eventually.
#' @export
saveToCache <- function(varName, startDate = NULL, endDate = NULL,
                        cacheDir = NULL, envir = parent.frame()) {
  stopifnot(!xor(is.null(startDate), is.null(endDate)))
  if (!is.null(startDate))
    vn <- getCacheVarDated(varName, startDate = startDate, endDate = endDate)
  else
    vn <- varName

  save(list = varName,
       envir = envir,
       file = findCacheFilePath(vn, cacheDir),
       compress = "xz")
  invisible(get(varName, envir = envir))
}

#' @title load data from cache
#' @template varName
#' @template startEndDate
#' @template cacheDir
#' @param force logical, whether to reload from source even if found in cache
#' @param envir environment in which to load, deafults to \code{parent.frame()}
#' @export
loadFromCache <- function(varName,
                          startDate = NULL, endDate = NULL,
                          cacheDir = NULL, force = FALSE,
                          envir = parent.frame()) {
  # getFromCache already (cheekily) loads into the given environment and returns
  # the data: loadFromCache just does this silently.
  invisible(getFromCache(varName = varName,
                         startDate = startDate, endDate = endDate,
                         cacheDir = cacheDir, force = force,
                         envir = envir))
}

#' @title getFromCache
#' @template varName
#' @template startEndDate
#' @template cacheDir
#' @param envir environment in which to save the retrieved cache data. This is
#'   done even though the data is being return, being a simple in-memory level
#'   of the cache. Default is \code{parent.frame()}.
#' @param force logical whether to get the data from cache even if it is already
#'   in an accessible environment
#' @export
getFromCache <- function(varName,
                         startDate = NULL, endDate = NULL,
                         cacheDir = NULL, force = FALSE,
                         envir = parent.frame()) {

  stopifnot(!xor(is.null(startDate), is.null(endDate)))
  if (!is.null(startDate))
    vn <- getCacheVarDated(varName, startDate = startDate, endDate = endDate)
  else
    vn <- varName

  if (!force && exists(varName, envir = envir, inherits = TRUE))
    return(get(varName, envir = envir, inherits = TRUE))

  fp <- findCacheFilePath(vn, cacheDir)
  if (!file.exists(fp))
    stop(sprintf("'%s' doesn't exist when trying to access cache", fp))

  vl <- load(file =  fp, envir = envir)
  # we are assuming that the .RData file contains a variable with the same name
  # as the file name (minus the file extension)
  #
  # if the data has a date range, then we use the undated variable name. Double
  # check this now.
  stopifnot(length(vl) == 1)
  stopifnot(varName == vl)
  #get(varName, envir = envir)
  get(vl, envir = envir)
}

#' @title assign a value to an environment, but only evaluate the assignment if
#'   it doesn't already exist on the cache. In this case, it is also saved in
#'   the cache.
#' @description Same as assign, but will only touch cache if the variable isn't
#'   already loaded.
#' @param value value to be lazy evaluated. Consider also enabling a call, or
#'   expression. \code{eval} could be called on it, so a constructed expression
#'   can be passed in. Since scoping changes, this should be self contained.
#' @template varName
#' @template startEndDate
#' @template cacheDir
#' @param envir environment to assign to
#' @param searchEnv environment (and parents) to search to see if we really need
#'   to load from cache
#' @param force logical value, if TRUE will force the assignment to overwrite
#'   whatever was in the cache, if anything.
#' @return invisibly returns the value assigned
#' @family cache
#' @export
assignCache <- function(value, varName,
                        startDate = NULL, endDate = NULL,
                        cacheDir = NULL,
                        envir = parent.frame(),
                        force = FALSE) {

  stopifnot(length(varName) == 1)
  stopifnot(is.character(varName))
  stopifnot(!xor(is.null(startDate), is.null(endDate)))
  stopifnot(length(startDate) <= 1)
  stopifnot(length(endDate) <= 1)
  stopifnot(is.null(startDate) || is.character(startDate))
  stopifnot(is.null(endDate) || is.character(endDate))
  stopifnot(is.environment(envir))
  stopifnot(is.logical(force))

  # "value" should not be evaluated until used, so a database query in 'value'
  # should be ignored if not needed, and not throw an error if database not
  # available.
  if (!force && isCached(varName = varName,
                          startDate = startDate, endDate = endDate,
                          cacheDir = cacheDir, envir = envir,
                          force = FALSE)) {
    return(loadFromCache(varName = varName, cacheDir = cacheDir,
                         startDate = startDate, endDate = endDate,
                         force = FALSE, envir = envir))
  }

  # this evaluates 'value' and should run the db query at this point
  assign(x = varName, value = value, envir = envir)
  saveToCache(varName = varName,
              startDate = startDate, endDate = endDate,
              cacheDir = cacheDir, envir = envir)

}

#' @rdname assignCache
#' @param fun function which accepts startDate and endDate arguments
#' @param ... arguments to pass on to \code{fun}
#' @export
assignCacheByFun <- function(fun, varName,
                             startDate = NULL, endDate = NULL, ...,
                             cacheDir = NULL,
                             envir = parent.frame(),
                             searchEnv = envir,
                             force = FALSE) {
  assignCache(value = fun(startDate = startDate, endDate = endDate, ...),
              varName = varName,
              startDate, endDate, cacheDir = cacheDir,
              envir = envir, searchEnv = searchEnv,
              force = force)

}

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
#' @template varName
#' @template cacheDir
#' @family cache
#' @export
findCacheFilePath <- function(varName, cacheDir = NULL) {
  cacheDir <- findCacheDir(cacheDir)
  if (!file.exists(cacheDir))
    stop("could not find cache directory: ", cacheDir)
  file.path(cacheDir, paste0(varName, ".RData"))
}

#' @title save data compressed in data folder
#' @description xz appears to fail on Windows, so use bzip2
#'
#'   tools::checkRdaFiles(file.path("data", list.files(path = "data")))
#'   tools::resaveRdaFiles(file.path("data", list.files(path = "data")),
#'   compress = "xz")
#' @template varName
#' @param suffix char additional characters before ".RData"
#' @family cache
#' @export
saveInDataDir <- function(varName, suffix) {
  save(list = varName,
       envir = parent.frame(),
       file = file.path('data', strip(paste0(varName, suffix, '.RData'))),
       compress = ifelse(platformIsWindows(), "bzip2", "xz")
  )
}

#' @title remove from cache and environment
#' @template varName
#' @template cacheDir
#' @param envir environment in which to remove the variable from, defaults to
#'   the calling frame.
#' @family cache
#' @export
rmCache <- function(varName = NULL, pattern = NULL, startDate = NULL, endDate = NULL,
                    cacheDir = NULL, envir = parent.frame()) {
  stopifnot(!xor(is.null(startDate), is.null(endDate)))
  stopifnot(xor(is.null(varName), is.null(pattern)))
  stopifnot(xor(is.null(startDate), is.null(pattern)))

  if (!is.null(varName)) {
    if (!is.null(startDate))
      vn <- getCacheVarDated(varName, startDate = startDate, endDate = endDate)
    else
      vn <- varName
    if (isCached(varName = varName, cacheDir = cacheDir,
                 force = TRUE, envir = envir)) {
      file.remove(findCacheFilePath(varName, cacheDir))
    }
    suppressWarnings(rm(list = c(varName, vn),
                        envir = envir, inherits = FALSE))
  } else {
    file.remove(lsCacheFiles(cacheDir = cacheDir, pattern = pattern))
    suppressWarnings(rm(list = ls(pattern = pattern, envir = envir)))
  }
}

#' @title list cache contents
#' @param pattern regex, ".Rdata$" is appended within the function for file name
#'   searches
#' @template cacheDir
#' @family cache
#' @export
lsCacheFiles <- function(cacheDir = NULL, pattern = ".*")
  list.files(path = findCacheDir(cacheDir = cacheDir),
             pattern = paste0(pattern, "\\.RData$"))

#' @rdname lsCacheFiles
#' @export
lsCache <- function(cacheDir = NULL, pattern = ".*")
  sub(pattern = "\\.RData", replacement = "", lsCacheFiles(cacheDir, pattern))

#' @title rename cache file(s)
#' @description renames a cache file, or set of dated files. It has the ability
#'   to rename a single dated file, if the dated name is given, but probably
#'   best to keep all the files for the same root name in sync. The trick is not
#'   just renaming the files, but renaming the variable names of the data
#'   therein.
#' @param oldName character vector length one
#' @param newName character vector length one
#' @param cacheDir character vector length one
#' @param envir environment to use
#' @family cache
#' @export
renameCache <- function(oldVar, newVar,
                        cacheDir = NULL, envir = parent.frame(),
                        verbose = TRUE) {

  stopifnot(is.null(cacheDir) || length(cacheDir) == 1)
  stopifnot(is.environment(envir))
  stopifnot(exists(oldVar, envir))
  stopifnot(!exists(newVar, envir)) # maybe allow overwrite?

  inmem <- ls(pattern = paste0("^", oldVar), envir = envir)
  ondisk <- grep(pattern = paste0("^", oldVar),
                 lsCache(cacheDir = cacheDir),
                 value = TRUE)
  # actually, I'm leaning towards having
  #only the un-dated var name in memory, and the only dated file names on disk
  #stopifnot(length(inmem) == length(ondisk))
  #stopifnot(all(inmem %in% ondisk))

  stopifnot(length(ondisk) > 0)

  # for each cached file, load into work env, assign data to new var name
  for (vo in ondisk) {
    vn <- sub(pattern = oldVar, replacement = newVar, x = vo)
    loadFromCache(varName = vo, cacheDir = cacheDir, force = TRUE)
    saveToCache(varName = vn, cacheDir = cacheDir)
    if (verbose) message(sprintf("removing varName '%s' from cache", vo))
    rmCache(varName = vo, cacheDir = cacheDir, envir = envir)
  }

  # the memory 'cache' is never dated, just the var name, so a bit simpler. if
  # there were multiple dated var names in the cache dir, we don't know which
  # was in memory, so we should just delete them.
  for (vo in inmem) {
    vn <- sub(pattern = oldVar, replacement = newVar, x = vo)
    assign(vn, get(vo, envir = envir), envir = envir)
  }

  if (length(ondisk) == 1) {
    vn <- sub(pattern = oldVar, replacement = newVar, x = ondisk)
    assign(vn, getFromCache(varName = vn, cacheDir = cacheDir, force = TRUE))
  }
  rm(list = inmem, envir = envir)
}

#' @title make cache file name from dates and unadorned variable name
#' @template varName
#' @template startEndDate
#' @family cache
#' @export
getCacheVarDated <- function(varName, startDate, endDate) {
  stopifnot(length(varName) == length(startDate))
  stopifnot(length(endDate) == length(startDate))
  paste(varName,
        paste(startDate,
              endDate, sep = "to"),
        sep = "")
}
