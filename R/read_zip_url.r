#' @title read file from zip at URL
#' @description downloads zip file, and opens named file \code{filename}, or the
#'   single file in zip if \code{filename} is not specified. FUN is a function,
#'   with additional arguments to FUN given by \dots.
#'   @details TODO: update from \code{icd} package
#' @param url character vector of length one containing URL of zip file.
#' @param filename character vector of length one containing name of file to
#'   extract from zip. If not specified, and the zip contains a single file,
#'   then this single file will be used.
#' @param FUN function used to process the file in the zip, defaults to
#'   readLines. The first argument to FUN will be the path of the extracted
#'   \code{filename}
#' @param \dots further arguments to FUN
#' @export
read_zip_url <- function(url, filename = NULL, FUN = readLines, ...) {
  stopifnot(length(filename) <= 1)
  stopifnot(is.character(url), length(url) == 1)
  zipfile <- tempfile()
  on.exit(unlink(zipfile), add = TRUE)
  utils::download.file(url = url, destfile = zipfile, quiet = TRUE)
  zipdir <- tempfile()
  on.exit(unlink(zipfile, recursive = TRUE), add = TRUE)
  dir.create(zipdir)
  utils::unzip(zipfile, exdir = zipdir)  # files="" so extract all
  files <- list.files(zipdir, recursive = TRUE)
  if (is.null(filename)) {
    if (length(files) == 1)
      filename <- files
    else
      stop("multiple files in zip, but no filename specified: ",
           paste(files, collapse = ", "))
  } else
    stopifnot(filename %in% files)
  do.call(FUN, args = c(list(file.path(zipdir, filename), warn = FALSE),
                        list(...)))
}

#' @rdname read.zip.url
#' @export
read.zip.url <- read_zip_url
