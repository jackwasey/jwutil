#' @title option name for cache
#' @description allow use of \code{options} to search for the cache directory.
#' @keywords character internal
optName = "cachedir"

#' @title check whether an object is in the cache
#' @rdname cache
#' @description use the same search algorithm as loading from the cache, but
#'   only report whether the file was there.
#' @param varName character string containing the name of the cache object to
#'   load. This would be the variable name were it to be loaded. cacheDir = NULL
#' @param cacheDir specify cached dir. If NULL or not specified, then we search
#'   up through parents looking for the cache.
#' @param force single logical. If TRUE, then actually look in the cache
#'   directory, otherwise we are satisfied if the variable already exists in the
#'   given environment. This exactly parallels \code{loadFromCache}
#' @param envir environment in which to check whether the data is already loaded
#'   (force = TRUE will skip this test). Default is \code{.GlobalEnv}
#' @return logical, single logical value.
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
#' @rdname cache
#' @description There are various memoise and cache functions in R but none did
#'   what I wanted. These functions allow a package to cache data in a standard
#'   place, or specified directory.
#'
#'   Global environment is used by default to save
#' @param varName char
#' @param cacheDir char
#' @param force logical - reload data even if already available
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

#' @rdname cache
#' @export
lsCache <- function(cacheDir = NULL) {
  list.files(path = findCacheDir(cacheDir))
}

#' @rdname cache
#' @export
loadFromCache <- function(varName, cacheDir = NULL, force = FALSE, envir = .GlobalEnv) {
  # getFromCache loads into the given environment and returns the data:
  # loadFromCache just does this silently.
  invisible(getFromCache(varName, cacheDir, force, envir))
}

#' @rdname cache
#' @export
getFromCache <- function(varName, cacheDir = NULL, force = FALSE, envir = .GlobalEnv) {
  if (!force && exists(varName, envir = envir))
    return(get(varName, envir = envir, inherits = TRUE))

  fp <- findCacheFilePath(varName, cacheDir)
  if (!file.exists(fp)) stop(sprintf("Path '%s' doesn't exist when trying to access cache", fp))
  load(file =  fp, envir = envir)
  # we are assuming that the .RData file contains a variable with the same name
  # as the file name (minus the file extension)
  get(varName, envir = envir)
}

#' @title assign a value to an environment, but only evaluate the assignment if
#'   it doesn't already exist on the cache. In this case, it is also saved in
#'   the cache.
#' @rdname cache
#' @description Same as assign, but will only touch cache if the variable isn't
#'   already loaded.
#' @param envir environment to assign to
#' @param searchEnv environment (and parents) to search to see if we really need
#'   to load from cache
#' @param unlike assign, returns invisibly TRUE for 'did assign from cache', or
#'   FALSE when the cache had to be touched.
#' @param force logical value, if TRUE will force the assignment to overwrite
#'   whatever was in the cache, if anything.
#' @return TRUE for cache was written, FALSE for read-only.
#' @export
assignCache <- function(value, varName,
                        cacheDir = NULL,
                        envir = parent.frame(),
                        searchEnv = envir, force = FALSE) {
  # value should not be evaluated until used, so a database query in 'value'
  # should be ignored until needed.
  if (force || !isCached(varName, cacheDir = cacheDir, force = FALSE, envir = searchEnv)) {
    assign(varName, value, envir = envir) # this evaluates 'value' and should run the db query at this point
    saveToCache(varName, cacheDir = cacheDir, envir = envir)
    return(invisible(TRUE))
  }
  loadFromCache(varName, cacheDir, force = FALSE, envir = searchEnv)
  invisible(FALSE)
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
#' @param varName char
#' @param cacheDir char, defaults to NULL. If \code{cacheDir} is not \code{NULL}
#'   and doesn't exist, stop with error.
#' @param cacheDirName single character string, defaults to 'jwcache'. 'cache'
#'   alone is not distinctive, and conflicts with other things, such as the
#'   cache directory in the vignettes directory.
#' @import magrittr
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
#' @param varName char
#' @param cacheDir char, defaults to NULL. If \code{cacheDir} doesn't exist,
#'   stop with error.
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
#' @param varName char name of the variable in the calling frame to save
#' @param suffix char additional characters before ".RData"
#' @export
saveInDataDir <- function(varName, suffix) {
  save(list = varName,
       envir = parent.frame(),
       file = file.path('data', strip(paste0(varName, suffix, '.RData'))),
       compress = ifelse(platformIsWindows(), "bzip2", "xz")
  )
}

#' @rdname cache
#' @title remove from cache and environment
#' @export
rmCache <- function(varName, cacheDir = NULL, envir = parent.frame()) {
  if (isCached(varName = varName, cacheDir = cacheDir, force = TRUE, envir = envir)) {
    file.remove(findCacheFilePath(varName, cacheDir))
    suppressWarnings(rm(list = varName, envir = envir, inherits = FALSE))
  }
}
