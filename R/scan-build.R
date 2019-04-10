#' Build with current Makevars, but with clang scan-build static analysis
#'
#' C and C++ compilers are replaced by `scan-build clang`, and restored
#' afterwards. Other flags and anything else in `~/.R/Makevars` is left alone.
#' @param path Path to package root, default is \code{"."}.
#' @param clang Path or name of clang compiler executable. Currently `clang-8`
#'   which is what MacOS homebrew currently (early 2019) provides.
#' @param scan_build Path or name of scan-build executable. Current
#'   `scan-build`, also from MacOS Homebrew. On linux, this has the LLVM version
#'   suffix, e.g., `scan-build-8`.
#' @export
jw_scan_build <- function(path = ".",
                          clang = "clang-8",
                          scan_build = "scan-build") {
  pkgbuild::clean_dll(path = path)
  exe <- paste(scan_build, clang)
  withr::with_makevars(
    c("CXX" = exe,
      "CXX11" = exe,
      "CXX14" = exe,
      "CXX17" = exe
    ), {
      pkgbuild::compile_dll(path = path)
    })
}

.otool_l <- function(path) {
  system2(command = "otool",
          args = c("-L", path),
          stdout = TRUE
  )
}

.links_gnu <- function(path) {
  any(
    grep(
      "libstdc",
      .otool_l(path = path)
    )
  )
}

.links_libc <- function(path) {
  any(
    grep(
      "libc",
      .otool_l(path = path)
    )
  )
}
jw_check_std_lib <- function() {
  rcpp_path <- system.file(
    "libs",
    "Rcpp.so",
    package = "Rcpp"
  )
  rcpp_gnu <- .links_gnu(rcpp_path)
  rcpp_libc <- .links_libc(rcpp_path)
}
