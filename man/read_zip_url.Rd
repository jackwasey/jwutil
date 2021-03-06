% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_zip_url.r
\name{read_zip_url}
\alias{read_zip_url}
\alias{read.zip.url}
\title{read file from zip at URL}
\usage{
read_zip_url(url, filename = NULL, FUN = readLines, ...)

read.zip.url(url, filename = NULL, FUN = readLines, ...)
}
\arguments{
\item{url}{character vector of length one containing URL of zip file.}

\item{filename}{character vector of length one containing name of file to
extract from zip. If not specified, and the zip contains a single file,
then this single file will be used.}

\item{FUN}{function used to process the file in the zip, defaults to
readLines. The first argument to FUN will be the path of the extracted
\code{filename}}

\item{\dots}{further arguments to FUN}
}
\description{
downloads zip file, and opens named file \code{filename}, or the
  single file in zip if \code{filename} is not specified. FUN is a function,
  with additional arguments to FUN given by \dots.
  @details TODO: update from \code{icd} package
}
\section{Functions}{
\itemize{
\item \code{read.zip.url}: Deprecated
}}

