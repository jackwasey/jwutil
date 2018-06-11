# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of jwutil.
#
# jwutil is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# jwutil is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with jwutil If not, see <http:#www.gnu.org/licenses/>.

#' unzip a single file from URL
#'
#' take a single file from zip located at a given URL, unzip into temporary
#' directory, and copy to the given \code{save_path}
#' @param url URL of a zip file
#' @param file_name file name of the resource within the zip file
#' @param save_path file path to save the first file from the zip
#' @importFrom utils unzip
#' @export
unzip_single <- function(url, file_name, save_path) {
  zipfile <- tempfile()
  # using libcurl because it seems the internal method works inconsistently
  curl_cap <- capabilities("libcurl")
  if (length(curl_cap) > 0 && curl_cap)
    method <- "libcurl"
  else
    method <- "auto"
  dl_code <- utils::download.file(url = url, destfile = zipfile,
                                  quiet = TRUE, method = method, mode = "wb")
  stopifnot(dl_code == 0)
  zipdir <- tempfile() # i do want tempfile, so I get an empty new directory
  dir.create(zipdir)
  utils::unzip(zipfile, exdir = zipdir)  # files="" so extract all
  files <- list.files(zipdir)
  if (is.null(file_name)) {
    if (length(files) == 1) {
      file_name <- files
    } else {
      stop("multiple files in zip, but no file name specified: ",
           paste(files, collapse = ", "))
    }
  } else
    stopifnot(file_name %in% files)
  ret <- file.copy(file.path(zipdir, file_name), save_path, overwrite = TRUE)
  unlink(zipdir, recursive = TRUE)
  ret
}

#' Save given variable in package data directory
#'
#' File is named varname.RData with an optional suffix before .RData
#'
#' @param var_name character or symbol, e.g. "myvar" or \code{myvar}, either of
#'   which would find \code{myvar} in the parent environment, and save it as
#'   \code{myvar.RData} in \code{package_root/data}.
#' @param suffix character scalar
#' @param data_path path to data directory, default is data in current directory.
#' @param package_dir character containing the directory root of the package
#'   tree in which to save the data. Default is the current working directory.
#' @param envir environment in which to look for the variable to save
#' @return invisibly returns the data
#' @export
save_in_data_dir <- function(var_name, suffix = "", data_path = "data",
                             package_dir = getwd(), envir = parent.frame()) {
  stopifnot(is.character(suffix) && length(suffix) == 1L)
  stopifnot(is.character(var_name) && length(var_name) == 1L)
  stopifnot(exists(var_name, envir = envir))
  var_name <- as.character(substitute(var_name))
  stopifnot(exists(var_name, envir = envir))
  save(list = var_name, envir = envir, compress = "xz",
       file = file.path(package_dir, data_path,
                        strip(paste0(var_name, suffix, ".RData"))))
  message("Now reload package to enable updated/new data: ", var_name)
  invisible(get(var_name, envir = envir))
}

# tempdir optional hard stop, to make sure CRAN doesn't get polluted and throw
# the package out
.tempdir <- function(...) {
  if (identical(Sys.getenv("NOT_CRAN"), "true"))
    stop("CRAN must not have residual temp files or directories")
  tempdir(...)
}
