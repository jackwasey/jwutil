#' @title option name for cache
#' @description allow use of \code{options} to search for the cache directory.
#' @keywords character internal
optName = "cachedir"

#' @title Cache an R object
#' @rdname cache
#' @description There are various memoise and cache functions in R but none did what I wanted. These functions allow
#' a package to cache data in a standard place, or specified directory.
#' @param varName char
#' @param cacheDir char
#' @param force logical - reload data even if already available
#' @export
saveToCache <- function(varName, cacheDir = NULL) {
  save(list = varName,
       envir = parent.frame(),
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
loadFromCache <- function(varName, cacheDir = NULL, force = FALSE) {
  if (!force && exists(varName)) invisible(varName)

  fp <- findCacheFilePath(varName, cacheDir)
  if (file.exists(fp)) invisible(load(file =  fp, envir = parent.frame()))

}

#' @rdname cache
#' @export
getFromCache <- function(varName, cacheDir = NULL, force = FALSE) {
  if (!force && exists(varName)) return(get(varName))

  # load into parent frame with its own varName, then return the data. This way
  # it is memory-cached also for future gets.
  load(file =  findCacheFilePath(varName, cacheDir), envir = parent.frame())
  get(varName)
}

#' @title find path to the cache directory
#' @description Searches a few likely places:
#'  - directory specified in \code{cacheDir}
#'  - session-wide R option "cacheDir"
#'  - 'cache' directory within the working directory
#'  - 'cache' directory within the parent directory
#'  Fail with error if we have still not found it.
#' @param varName char
#' @param cacheDir char, defaults to NULL. If \code{cacheDir} is not \code{NULL} and doesn't exist, stop with error.
#' @export
findCacheDir <- function(cacheDir = NULL) {
  if (!is.null(cacheDir) && file.exists(cacheDir)) return(cacheDir)
  if (!is.null(getOption(optName)) && file.exists(getOption(optName))) return(getOption(optName))
  td <- file.path(getwd(), "cache")
  if (file.exists(td)) return(td)
  td <- file.path(getwd(), "..", "cache") # this is good when stuck in vignette sub-directory of a project
  if (file.exists(td)) return(td)
  td <- file.path(getwd(), "../..", "cache") # this is good when stuck in vignette sub-directory of a project
  if (file.exists(td)) return(td)
  stop("Could not find cache directory in working directory:", getwd())
}

#' @title find path to a file in cache directory
#' @param varName char
#' @param cacheDir char, defaults to NULL. If \code{cacheDir} doesn't exist, stop with error.
#' @export
findCacheFilePath <- function(varName, cacheDir = NULL) {
  cacheDir <- findCacheDir(cacheDir)
  if (!file.exists(cacheDir)) stop("could not find cache directory: ", cacheDir)
  file.path(cacheDir, paste0(varName, ".RData"))
}

#' @title save data compressed in data folder
#' @description xz appears to fail on Windows, so use bzip2
#'
#' tools::checkRdaFiles(file.path("data", list.files(path = "data")))
#' tools::resaveRdaFiles(file.path("data", list.files(path = "data")), compress = "xz")
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
