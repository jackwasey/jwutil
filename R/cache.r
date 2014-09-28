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
  load(file =  findCacheFilePath(varName, cacheDir), envir = parent.Frame())
  get(varName)
}

findCacheDir <- function(cacheDir = NULL) {
  if (!is.null(cacheDir) && file.exists(file.path(getwd(), "cache"))) return(cacheDir)
  if (!is.null(getOption(optName)) && file.exists(getOption(optName))) return(getOption(optName))
  td <- file.path(getwd(), "cache")
  if (file.exists(td)) return(td)
  td <- file.path(getwd(), "..", "cache") # this is good when stuck in vignette sub-directory of a project
  if (file.exists(td)) return(td)
  stop("Could not find cache directory in working directory:", getwd())
}

findCacheFilePath <- function(varName, cacheDir = NULL) {
  fp <- file.path(findCacheDir(cacheDir), paste0(varName, ".RData"))
  if (!file.exists(fp)) stop("could not find:", varName, "in path:", fp)
  fp
}
