% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standalone-get-data-dir.R
\name{get_data_dir}
\alias{get_data_dir}
\alias{list_path_filenames}
\title{Check if data directory exists and create a new directory if needed}
\usage{
get_data_dir(
  path = NULL,
  cache = FALSE,
  create = TRUE,
  ask = TRUE,
  appname = NULL,
  pkg = NULL,
  allow_null = TRUE,
  quiet = FALSE,
  call = caller_env()
)

list_path_filenames(
  path,
  fileext = NULL,
  pattern = NULL,
  full.names = TRUE,
  call = caller_env(),
  ...
)
}
\arguments{
\item{path}{Path to directory for use as data directory.}

\item{cache}{If \code{TRUE}, and path is \code{NULL} set path to
\code{\link[rappdirs:user_cache_dir]{rappdirs::user_cache_dir()}} (using value of pkg as appname). If path is
not \code{NULL}, the path is returned even if cache is \code{TRUE}.}

\item{create}{If \code{FALSE} and path does not exist, return path with a warning.
If \code{TRUE} and \code{\link[rlang:is_interactive]{rlang::is_interactive()}} is \code{TRUE}, ask user if directory
should be created. If the session not interactive and create is \code{TRUE}, a
new directory will be created.}

\item{ask}{If \code{TRUE}, create is \code{FALSE}, and session is interactive, ask to
create directory if the provided directory does not exist.}

\item{appname, pkg}{pkg is used if appname is NULL. Passed to
\code{\link[rappdirs:user_cache_dir]{rappdirs::user_cache_dir()}}}

\item{allow_null}{If \code{TRUE}, path is \code{NULL}, cache is \code{FALSE}, return the
\code{NULL} path value; defaults to \code{TRUE}.}

\item{quiet}{If \code{TRUE}, suppress informational messages.}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}

\item{fileext}{File extension. If supplied to \code{\link[=list_path_filenames]{list_path_filenames()}} and
pattern is \code{NULL}, only return file names matching this extension.}

\item{pattern}{an optional \link[base]{regular expression}.  Only file names
    which match the regular expression will be returned.}

\item{full.names}{a logical value.  If \code{TRUE}, the directory
    path is prepended to the file names to give a relative file path.
    If \code{FALSE}, the file names (rather than paths) are returned.}

\item{...}{Additional parameters passed to \code{\link[=list.files]{list.files()}} by
\code{\link[=list_path_filenames]{list_path_filenames()}}.}
}
\description{
Get the path for a package-specific cache directory with
\code{\link[rappdirs:user_cache_dir]{rappdirs::user_cache_dir()}}, check for the existence of a data directory,
optionally create a new directory at the provided path location.
}
