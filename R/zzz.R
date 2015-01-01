.onLoad <- function(libname, pkgname){
  if (is.null(getOption("jwutil.cacheDirName"))) {
    options(jwutil.cacheDirName = "jwcache")
  }
  if (is.null(getOption("jwutil.fallbackCacheDir"))) {
    options(jwutil.fallbackCacheDir =
              paste0(tempdir(), getOption("jwutil.cacheDirName")))
  }
}
