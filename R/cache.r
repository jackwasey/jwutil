#' @title Cache an R object
#' @description There are various memoise and cache functions in R but none did what I wanted. These functions allow
#' a package to cache data in a standard place, or specified directory.
#' @param varName char
#' @param cacheDir char
#' @export
saveToCache <- function(varName, cacheDir = file.path(getwd(), "cache")) {
  save(list = varName, envir = parent.frame(), file = file.path(cacheDir, paste0(varName, ".RData")), compress = "xz") 
}

#' @rdname saveToCache
#' @export
lsCache <- function( cacheDir = file.path(getwd(), "cache")) {
  list.files(path = cacheDir)
}

#' @rdname saveToCache
#' @export
loadFromCache <- function(varName, cacheDir = file.path(getwd(), "cache")) {
  load(file =  file.path(cacheDir, paste0(varName, ".RData")), envir = parent.frame())
}

#' @rdname saveTocache
#' @export
getFromCache <- function(varName, cacheDir = file.path(getwd(), "cache")) {
  te = new.env()
  load(file =  file.path(cacheDir, paste0(varName, ".RData")), envir = te)
  get(varName, envir = te)
}

