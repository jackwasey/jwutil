.onLoad <- function(libname, pkgname){
  if (is.null(getOption("jwutil.fallbackCacheDir"))) {
    # /tmp is not cross platform, and can't generate a temp dir in a static option.
    #message("Setting jwutil.fallbackCacheDir")
    options(jwutil.fallbackCacheDir = "~/jwcache")
  }
}
