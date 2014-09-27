#' @title Cache an R object
#' @rdname cache
#' @description There are various memoise and cache functions in R but none did what I wanted. These functions allow
#' a package to cache data in a standard place, or specified directory.
#' @param varName char
#' @param cacheDir char
#' @export
saveToCache <- function(varName, cacheDir = file.path(getwd(), "cache")) {
  save(list = varName, envir = parent.frame(), file = file.path(cacheDir, paste0(varName, ".RData")), compress = "xz")
}

#' @rdname cache
#' @export
lsCache <- function( cacheDir = file.path(getwd(), "cache")) {
  list.files(path = cacheDir)
}

#' @rdname cache
#' @export
loadFromCache <- function(varName, cacheDir = file.path(getwd(), "cache")) {
  fp <- file.path(cacheDir, paste0(varName, ".RData"))
  if (file.exists(fp)) invisible(load(file =  fp, envir = parent.frame()))
  fp <- file.path(getwd(), "cache", paste0(varName, ".RData"))
  if (file.exists(fp)) invisible(load(file =  fp, envir = parent.frame()))
  fp <- file.path(getwd(), "..", "cache", paste0(varName, ".RData"))
  if (file.exists(fp)) invisible(load(file = fp, envir = parent.frame()))
}

#' @rdname cache
#' @export
getFromCache <- function(varName, cacheDir = file.path(getwd(), "cache")) {
  te = new.env()
  load(file =  file.path(cacheDir, paste0(varName, ".RData")), envir = te)
  get(varName, envir = te)
}

