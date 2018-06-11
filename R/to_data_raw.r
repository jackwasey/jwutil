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
  stopifnot(is.character(url) && length(url) == 1L)
  stopifnot(is.character(file_name) && length(file_name) == 1L)
  stopifnot(is.logical(offline) && length(offline) == 1L)
  if (!dir.exists(data_raw_path)) data_raw_path <- .tempdir()
  file_path <- file.path(data_raw_path, make.names(file_name))
  if (verbose) {
    message("file path = ", file_path)
    message("file name = ", file_name)
  }
  if (force || !file.exists(file_path)) {
    if (offline) return()
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
  stopifnot(is.character(url) && length(url) == 1L)
  stopifnot(is.character(file_name) && length(file_name) == 1L)
  stopifnot(is.logical(offline) && length(offline) == 1L)
  if (!dir.exists(data_raw_path)) data_raw_path <- .tempdir()
  save_path <- file.path(data_raw_path, file_name)
  f_info <- list(file_path = save_path, file_name = file_name)
  if (file.exists(save_path)) return(f_info)
  if (offline) return()
  if (utils::download.file(url = url, destfile = save_path, quiet = TRUE) != 0)
    stop(paste(url, " not downloaded successfully."))
  f_info
}
