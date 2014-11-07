#' @title option name for cache
#' @description allow use of \code{options} to search for the cache directory.
#' @keywords character internal
optName = "cachedir"

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
#' @param load_envir target environment in which to load given cached data.
#'   Default is \code{envir}
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
loadFromCache <- function(varName, cacheDir = NULL, force = FALSE,
                          envir = .GlobalEnv, load_envir = envir) {
  if (!force && exists(varName))
    return(invisible(get(varName, envir = envir, inherits = TRUE)))

  fp <- findCacheFilePath(varName, cacheDir)
  if (file.exists(fp)) {
    load(file =  fp, envir = load_envir)
    return(invisible(get(varName, envir = load_envir)))
  }
  stop("couldn't find '", varName, "' in path ", fp)
}

#' @rdname cache
#' @export
getFromCache <- function(varName, cacheDir = NULL, force = FALSE,
                         load_envir = .GlobalEnv) {
  if (!force && exists(varName)) return(get(varName, envir = load_envir, inherits = TRUE))

  # load into parent frame with its own varName, then return the data. This way
  # it is memory-cached also for future gets. Alternative is to memoise.
  load(file = findCacheFilePath(varName, cacheDir), envir = load_envir) # or parent.frame() ???
  get(varName, envir = load_envir)
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
#' @export
findCacheDir <- function(cacheDir = NULL, cacheDirName = "jwcache") {
  if (!is.null(cacheDir) && file.exists(cacheDir)) return(cacheDir)
  if (!is.null(getOption(optName)) && file.exists(getOption(optName))) return(getOption(optName))
  td <- file.path(getwd(), cacheDirName)
  if (file.exists(td)) return(td)
  td <- file.path(dirname(getwd()), cacheDirName) # this is good when stuck in vignette sub-directory of a project
  if (file.exists(td)) return(td)
  td <- file.path(dirname(dirname(getwd())), cacheDirName) # parent of parent
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
  message("Could not find cache directory starting from working directory: ", getwd())
  if (platformIsLinux())
    system(command = sprintf("locate --regex  %s$", cacheDirName), intern = TRUE) %>%
    paste(sep=", ", collapse=", ") %>% message
  message("You can use the cacheDir= argument to specify it directly,
          or check the cache was created in the correct place")
  fb <- options("jwutil.fallbackCacheDir")
  message("using fallback cache directory: ")
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
       compress = "bzip2"
  )
}
