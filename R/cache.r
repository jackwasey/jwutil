#' @title option name for cache
#' @description allow use of \code{options} to search for the cache directory.
#' @family cache
#' @keywords character internal
optName = "cachedir"

#' @title check whether an object is in the cache
#' @description use the same search algorithm as loading from the cache, but
#'   only report whether the file was there.
#' @template varName
#' @template cacheDir
#' @param force single logical. If TRUE, then actually look in the cache
#'   directory, otherwise we are satisfied if the variable already exists in the
#'   given environment. This exactly parallels \code{loadFromCache}
#' @param envir environment in which to check whether the data is already loaded
#'   (force = TRUE will skip this test). Default is \code{.GlobalEnv}
#' @return logical, single logical value.
#' @family cache
#' @export
isCached <- function(varName, cacheDir = NULL, force = FALSE, envir = .GlobalEnv) {
  stopifnot(length(varName) == 1,
            length(cacheDir) == 1 || is.null(cacheDir),
            length(force) == 1)
  stopifnot(is.character(varName),
            is.logical(force),
            is.environment(envir))
  stopifnot(is.null(cacheDir) || is.character(cacheDir))

  if (!force && exists(varName, envir = envir)) return(TRUE)
  if (findCacheFilePath(varName, cacheDir) %>% file.exists) return(TRUE)
  FALSE
}

#' @title Cache an R object
#' @family cache
#' @description There are various memoise and cache functions in R but none did
#'   what I wanted. These functions allow a package to cache data in a standard
#'   place, or specified directory.
#'
#'   Global environment is used by default to save
#' @template varName
#' @template cacheDir
#' @param envir environment to start searching for the cached data (it may
#'   already be in memory). Starts off with \code{parent.frame()} by default, and
#'   /code{inherits}, so should find already loaded cache files in .GlobalEnv
#'   eventually.
#' @export
saveToCache <- function(varName, cacheDir = NULL, envir = parent.frame()) {
  save(list = varName,
       envir = envir,
       file = findCacheFilePath(varName, cacheDir),
       compress = "xz")
}

#' @title lsCache
#' @template cacheDir
#' @export
lsCache <- function(cacheDir = NULL) {
  list.files(path = findCacheDir(cacheDir))
}

#' @title loadFromCache
#' @template varName
#' @template cacheDir
#' @param force logical, whether to reload from source even if found in cache
#' @param envir environment in which to load, deafults to \code{.GlobalEnv}
#' @export
loadFromCache <- function(varName, cacheDir = NULL, force = FALSE, envir = .GlobalEnv) {
  # getFromCache already (cheekily) loads into the given environment and returns the data:
  # loadFromCache just does this silently.
  invisible(getFromCache(varName = varName, cacheDir = cacheDir, force = force, envir = envir))
}

#' @title load or get a dated variable from cache
#' @description This is a bit tricky with environments. The basic
#'   jwutil::getFromCache etc functions put the loaded data in the global
#'   environment by default. Here we are just going to load the (unddated name)
#'   data to the parent environment (by default), leaving the dated data in the global
#'   environment.
#' @template varName
#' @template startEndDate
#' @param envir, where to load the data, defaults to \code{.GlobalEnv}
#' @param ... additional arguments to pass to \code{getDatedFromCache}
#' @family cache
#' @export
loadDatedFromCache <- function(varName, startDate, endDate, envir = parent.env(), ...)
  assign(varName, getDatedFromCache(varName, startDate, endDate, envir = envir, ...), envir = envir)

#' @title getFromCache
#' @template varName
#' @template cacheDir
#' @param envir environment in which to save the retrieved cache data. This is
#'   done even though the data is being return, being a simple in-memory level
#'   of the cache. Default is \code{.GlobalEnv}.
#' @param force logical whether to get the data from cache even if it is already
#'   in an accessible environment
#' @export
getFromCache <- function(varName, cacheDir = NULL, envir = .GlobalEnv, force = FALSE) {
  if (!force && exists(varName, envir = envir))
    return(get(varName, envir = envir, inherits = TRUE))

  fp <- findCacheFilePath(varName, cacheDir)
  if (!file.exists(fp)) stop(sprintf("Path '%s' doesn't exist when trying to access cache", fp))
  load(file =  fp, envir = envir)
  # we are assuming that the .RData file contains a variable with the same name
  # as the file name (minus the file extension)
  get(varName, envir = envir)
}

#' @title get a variable for a given date range
#' @description we want to be transparent about the date range in the processing
#'   code, so we can easily change the input data without changing everything.
#'   We also want to cache data for some fixed date ranges, but these cached
#'   data files will have specific names. The solution is to use this function
#'   instead of \code{get} to bring the data into the working environment with a
#'   standardized name. TODO: should this go in jwutil?
#' @template varName
#' @template startEndDate
#' @param ... additional arguments pased to \code{get}
#' @family cache
#' @export
getDated <- function(varName, startDate, endDate, ...)
  get(getCacheFileName(varName, startDate, endDate), ...)

#' @title getDatedFromCache
#' @template varName
#' @template startEndDate
#' @param ... additional arguments pased to \code{getFromCache}
#' @export
getDatedFromCache <- function(varName, startDate, endDate, ...)
  getFromCache(varName = getCacheFileName(varName, startDate, endDate), ...)

#' @title assign a value to an environment, but only evaluate the assignment if
#'   it doesn't already exist on the cache. In this case, it is also saved in
#'   the cache.
#' @description Same as assign, but will only touch cache if the variable isn't
#'   already loaded.
#' @param value val
#' @template varName
#' @template cacheDir
#' @param envir environment to assign to
#' @param searchEnv environment (and parents) to search to see if we really need
#'   to load from cache
#' @param force logical value, if TRUE will force the assignment to overwrite
#'   whatever was in the cache, if anything.
#' @return unlike assign, returns invisibly TRUE for 'did assign from cache', or
#'   FALSE when the cache had to be touched.
#' @family cache
#' @export
assignCache <- function(value, varName,
                        cacheDir = NULL,
                        envir = parent.frame(),
                        searchEnv = envir,
                        force = FALSE) {

  if (is.null(force)) force <- FALSE

  # "value" should not be evaluated until used, so a database query in 'value'
  # should be ignored if not needed, and not throw an error if database not available.
  if (force || !isCached(varName, cacheDir = cacheDir, force = FALSE, envir = searchEnv)) {
    assign(x = varName, value = value, envir = envir) # this evaluates 'value' and should run the db query at this point
    saveToCache(varName, cacheDir = cacheDir, envir = envir)
    return(invisible(TRUE))
  }
  loadFromCache(varName, cacheDir, force = FALSE, envir = searchEnv)
  invisible(FALSE)
}

#' @title  assign dated to cache
#' @param value value to assign to the given variable called \code{varName}
#' @template varName
#' @template startEndDate
#' @template cacheDir
#' @param envir environment to assign to, defaults to \code{parent.frame()}
#' @param searchEnv environment in which to look to see if cache needs to be
#'   hit, defaults to \code{envir}
#' @param force logical, whether to re-evaluate and assign to cache even if data
#'   already exists
#' @export
assignDatedCache <- function(value, varName,
                             startDate, endDate,
                             cacheDir = NULL, envir = parent.frame(), searchEnv = envir, force = FALSE) {
  assignCache(value = value,
              varName = getCacheFileName(varName, startDate, endDate),
              cacheDir, envir, searchEnv, force)

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
findCacheDir <- function(cacheDir = NULL, cacheDirName = "jwcache", verbose = FALSE) {
  if (!is.null(cacheDir) && file.exists(cacheDir)) return(cacheDir)
  if (!is.null(getOption(optName)) && file.exists(getOption(optName))) return(getOption(optName))
  td <- file.path(getwd(), cacheDirName)
  if (file.exists(td)) return(td)

  # parents: this is good when stuck e.g. in vignette sub-directory of a project
  td <- getwd() %>% dirname %>% file.path(cacheDirName)
  if (file.exists(td)) return(td)
  td <- getwd() %>% dirname %>% dirname %>% file.path(cacheDirName) #parent of parent
  if (file.exists(td)) return(td)
  td <- getwd() %>% dirname %>% dirname %>% dirname %>% file.path(cacheDirName) # parent of parent of parent
  if (file.exists(td)) return(td)
  td <- getwd() %>%
    dirname %>% dirname %>% dirname %>% dirname %>%
    file.path(cacheDirName) # parent of parent of parent of parent (believe it or not, this has use cases)
  if (file.exists(td)) return(td)

  # now we've looked where it should be, and still not found it, let's keep
  # stepping up directories and recursively searching down. This is a terrible idea, very slow.
  #   pwd <- getwd()
  #   lastwd <- pwd
  #   repeat {
  #     if ()
  #     message("searching path: ", pwd)
  #     td <- list.dirs(path = pwd, recursive = T) %>% grep(pattern = cacheDirName, value = TRUE)
  #     if (length(td) == 1) return(td)
  #     if (length(td)  > 1) {
  #       warning("found multiple matching cache paths:", td)
  #       return(td[1])
  #     }
  #     # length = 0 i.e. no subdirs in pwd matching the cache dir name
  #     lastwd <- pwd
  #     pwd <- dirname(pwd)
  #     # can't go higher than root. Probably undesirable to search all the way up
  #     # to root...
  #     if (pwd == lastwd) break
  #   }
  if (verbose) message("Could not find cache directory starting from working directory: ", getwd())
  if (verbose && platformIsLinux())
    system(command = sprintf("locate --regex  %s$", cacheDirName), intern = TRUE) %>%
    paste(sep=", ", collapse=", ") %>% message
  if (verbose) message("You can use the cacheDir= argument to specify it directly,
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
  if (!file.exists(cacheDir)) stop("could not find cache directory: ", cacheDir)
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
rmCache <- function(varName, cacheDir = NULL, envir = parent.frame()) {
  if (isCached(varName = varName, cacheDir = cacheDir, force = TRUE, envir = envir)) {
    file.remove(findCacheFilePath(varName, cacheDir))
    suppressWarnings(rm(list = varName, envir = envir, inherits = FALSE))
  }
}

#' @title make cache file name from dates and unadorned variable name
#' @template varName
#' @template startEndDate
#' @family cache
#' @export
getCacheFileName <- function(varName, startDate, endDate) {
  stopifnot(length(varName) == length(startDate))
  stopifnot(length(endDate) == length(startDate))
  paste(varName,
        paste(startDate,
              endDate, sep = "to"),
        sep = "")
}
