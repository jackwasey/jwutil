# Copyright (C) 2014 - 2017  Jack O. Wasey
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
#' @export
unzip_single <- function(url, file_name, save_path) {
  checkmate::assert_character(url, len = 1)
  checkmate::assert_character(file_name, len = 1)
  checkmate::assert_character(save_path, len = 1)
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

#' Unzip file to \code{data-raw}
#'
#' Get a zip file from a URL, extract contents, and save file in
#' \code{data-raw}. If the file already exists there, it is only retrieved if
#' \code{force} is set to \code{TRUE}. If \code{offline} is \code{FALSE}, then
#' \code{NULL} is returned if the file isn't already downloaded.
#'
#' The file name is changed to a conservative cross platform name using
#' \code{make.names}
#'
#' @param url URL of a zip file
#' @param file_name file name of a single file in that zip
#' @param force logical, if TRUE, then download even if already in
#'   \code{data-raw}
#' @template verbose
#' @template offline
#' @param data_raw_path path where the data-raw directory is.
#' @return path of unzipped file in \code{data-raw}
#' @export
unzip_to_data_raw <- function(url, file_name, force = FALSE, verbose = FALSE,
                              offline = TRUE, data_raw_path = "data-raw") {
  checkmate::assert_string(url, na.ok = FALSE)
  checkmate::assert_string(file_name, na.ok = FALSE)
  checkmate::assert_flag(offline)

  if (!dir.exists(data_raw_path)) {
    stop("creating temp dir - must delete after test if done on CRAN")
    data_raw_path <- tempdir()
  }

  file_path <- file.path(data_raw_path, make.names(file_name))
  if (verbose) {
    message("file path = ", file_path)
    message("file name = ", file_name)
  }
  if (force || !file.exists(file_path)) {
    if (offline)
      return()
    stopifnot(
      unzip_single(url = url, file_name = file_name, save_path = file_path)
    )
  }
  list(file_path = file_path, file_name = make.names(file_name))
}

#' @rdname unzip_to_data_raw
#' @export
download_to_data_raw <- function(
  url,
  file_name = regmatches(url, regexpr("[^/]*$", url)),
  offline = TRUE, data_raw_path = "data-raw") {
  checkmate::assert_string(url)
  checkmate::assert_string(file_name)
  checkmate::assert_flag(offline)

  if (!dir.exists(data_raw_path)) {
    stop("creating temp dir - must delete after test if done on CRAN")
    data_raw_path <- tempdir()
  }

  save_path <- file.path(data_raw_path, file_name)
  f_info <- list(file_path = save_path, file_name = file_name)

  if (file.exists(save_path))
    return(f_info)

  if (offline)
    return()

  # consider libcurl, but seems to work without now
  if (utils::download.file(url = url, destfile = save_path, quiet = TRUE, method = "auto") != 0)
    stop(paste(url, " not downloaded successfully."))

  f_info

}

#' Save given variable in package data directory
#'
#' File will be named \emph{var_name}\code{.RData} with an optional suffix
#' before \code{.RData}
#'
#' @param var_name character or symbol, e.g. \code{"myvar"} or \code{myvar},
#'   either of which would find \code{myvar} in the parent environment, and save
#'   it as \code{myvar.RData} in \code{package_root/data}.
#' @param suffix character scalar
#' @param data_path path to data directory, default is data in current directory.
#' @return invisibly returns the data
#' @keywords internal file
save_in_data_dir <- function(var_name, suffix = "", data_path = "data") {
  checkmate::assert_string(suffix)
  var_name <- as.character(substitute(var_name))
  stopifnot(exists(var_name, envir = parent.frame()))
  save(list = var_name,
       envir = parent.frame(),
       file = file.path(data_path, strip(paste0(var_name, suffix, ".RData"))),
       compress = "xz")
  message("Now reload package to enable updated/new data: ", var_name)
  invisible(get(var_name, envir = parent.frame()))
}
