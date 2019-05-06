#' Compile and test package with and without profile guided optimization
#'
#' Running
#' the test suite is a proxy for some kind of benchmark. Obviously this is
#' inadequate with most CRAN test-suites. However, the \code{test_that} path may
#' be set to a directory of \pkg{testthat} tests, which are called by
#' \code{\link[testthat]{test_dir}}. Alternatively, environment variables or \R
#' options may be set to run computationally intensive tests which exercise the
#' compiled code.
#'
#' Do not use 'ccache'!
#'
#' @keywords internal
#' @export
pgo_bench <- function(
                      pkg_path = getwd(),
                      test_path = "tests/testthat",
                      prof_dir = file.path(pkg_path, "src/prof"),
                      iterations = 7,
                      quiet = TRUE,
                      reporter = ifelse(quiet, "silent", "check"),
                      clean = FALSE,
                      opt_level = "-O3",
                      cc = "clang",
                      cxx = "clang++",
                      brew = Sys.info()[["sysname"]] == "Darwin",
                      measure = c("real", "user", "system"),
                      ...) {
  measure <- match.arg(measure)
  llvm_bin_path <- "/usr/local/opt/llvm/bin"
  if (brew && dir.exists(llvm_bin_path)) {
    cc <- file.path(llvm_bin_path, cc)
    cxx <- file.path(llvm_bin_path, cxx)
  }
  # three tests:
  #  - 1. regular (show/set optimization flags)
  #  - 2. pgo generate (don't need to time test results)
  #  - 3. pgo use (compare time for this to 1.)
  prof_out <- file.path(prof_dir, "default.profdata")
  dir.create(prof_dir, showWarnings = FALSE)
  if (!quiet) message("saving profiles to: ", prof_dir)
  if (clean) on.exit(unlink(prof_dir), add = TRUE)
  # clang doesn't take profie-dir
  # gen_end <- paste0("-fprofile-generate -fprofile-dir=", prof_dir)
  # use_end <- paste0("-fprofile-use -fprofile-dir=", prof_dir)
  # is this just the clang way?
  gen_end <- paste0(
    "-fprofile-generate=", prof_dir,
    " -Wno-unused-variable"
  )
  use_end <- paste0("-fprofile-use=", prof_out)
  gen_end_ld <- gen_end
  use_end_ld <- use_end
  gen_flags <- paste(opt_level, gen_end)
  # -fprofile-correct may be needed for multi-threaded applications
  use_flags <- paste(opt_level, use_end)
  mk_cc <- c(
    CC = cc,
    CXX = cxx,
    CXX11 = cxx
  )
  mk_gen <- c(
    mk_cc,
    CFLAGS = gen_flags,
    CXXFLAGS = gen_flags,
    CXX1XFLAGS = gen_flags,
    CXX11FLAGS = gen_flags,
    MAKEFLAGS = "-j1",
    FFLAGS = gen_flags,
    FCFLAGS = gen_flags,
    # LDFLAGS is ignored on windows and visa versa
    LDFLAGS = gen_end_ld
    # SHLIB_LIBADD = gen_end_ld)
  )
  mk_use <- c(
    mk_cc,
    CFLAGS = use_flags,
    CXXFLAGS = use_flags,
    CXX1XFLAGS = use_flags,
    CXX11FLAGS = use_flags,
    MAKEFLAGS = "-j1",
    FFLAGS = use_flags,
    FCFLAGS = use_flags,
    # LDFLAGS is ignored on windows and visa versa
    LDFLAGS = use_end_ld
    # SHLIB_LIBADD = use_end_ld
  )

  .fix_test_res <- function(tr) {
    as.data.frame(tr)[c(1:3, 11)]
  }

  iter_res <- function() {
    # assume tests are reported in the same order.
    out <- NULL
    for (i in seq_len(iterations)) {
      o <- .fix_test_res(
        testthat::test_dir(
          path = test_path,
          reporter = reporter,
          ...
        )
      )
      if (!is.null(out)) {
        out[[measure]] <- out[[measure]] + o[[measure]]
      } else {
        out <- o
      }
    }
    out[[measure]] <- out[[measure]] / iterations
    out
  }

  do_pre <- function() {
    pkgbuild::clean_dll()
    devtools::load_all(
      compile = TRUE,
      quiet = quiet
    )
    iter_res()
  }

  do_gen <- function() {
    withr::with_makevars(
      mk_gen, {
        pkgbuild::clean_dll()
        devtools::load_all(
          compile = TRUE,
          quiet = quiet
        )
        out <- iter_res()
      }
    )
    system2(
      "llvm-profdata",
      paste0(
        "merge",
        " -output=",
        prof_out,
        " ", prof_dir
      )
    )
    out
  }

  do_use <- function() {
    withr::with_makevars(
      mk_use, {
        pkgbuild::clean_dll()
        devtools::load_all(
          compile = TRUE,
          quiet = quiet
        )
        iter_res()
      }
    )
  }

  pgo_pre <- do_pre()
  # do generation just once
  do_gen()
  out <- pgo_use <- do_use()
  ratio_col_name <- paste0(measure, "_pgo_vs_not")
  out[[ratio_col_name]] <- pgo_use[[measure]] / pgo_pre[[measure]]
  out[[measure]] <- NULL
  out <- out[order(out[[ratio_col_name]], decreasing = TRUE), ]
  rownames(out) <- NULL
  out
}
