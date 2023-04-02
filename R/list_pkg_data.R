#' List package datasets, extdata files, and files in package cache
#'
#' [list_pkg_data()] calls three helper functions and returns a data.frame.
#' [list_pkg_datasets()] lists package datasets (using the library path as the
#' path), [list_pkg_extdata()] lists extdata files for a package (using basename
#' without a file extension as the item name), [list_pkg_cachedata()] lists data
#' in the package cache folder returned by [rappdirs::user_cache_dir()].
#'
#' @inheritParams rlang::check_installed
#' @export
#' @importFrom rlang caller_env check_installed
list_pkg_data <- function(pkg = NULL,
                          version = NULL,
                          dir = "extdata",
                          lib.loc = NULL,
                          call = caller_env()) {
  rlang::check_installed(pkg, version = version, call = call)

  rbind(
    list_pkg_datasets(pkg, lib.loc = lib.loc),
    list_pkg_extdata(pkg, dir = dir),
    list_pkg_cachedata(pkg)
  )
}

#' @name list_pkg_datasets
#' @rdname list_pkg_data
#' @inheritParams base::system.file
#' @export
#' @importFrom utils data
#' @importFrom rlang set_names
list_pkg_datasets <- function(pkg,
                              lib.loc = NULL) {
  check_string(pkg)
  data_files <-
    as.data.frame(
      readRDS(system.file("Meta", "data.rds", package = pkg, lib.loc = lib.loc))
    )

  data_files <-
    cbind(
      data_files[, c(2, 1)],
      rep_len(pkg, nrow(data_files))
    )

  rlang::set_names(data_files, c("item", "path", "package"))
}

#' @name list_pkg_extdata
#' @rdname list_pkg_data
#' @param dir One or more directory names to pass as the first argument of
#'   [system.file()]. Defaults to "extdir".
#' @inheritParams base::list.files
#' @export
#' @importFrom rlang is_empty
list_pkg_extdata <- function(pkg,
                             dir = "extdata",
                             full.names = TRUE,
                             recursive = TRUE) {
  check_string(pkg)
  if (length(dir) > 1) {
    list_system_files <- Vectorize(list_system_files, "dir", TRUE, FALSE)
  }

  extdata_files <- list_system_files(pkg, dir, recursive, full.names)

  if (!rlang::is_empty(extdata_files)) {
    extdata_files <-
      cbind(
        data.frame("item" = str_remove_fileext(basename(extdata_files))),
        data.frame("path" = extdata_files),
        data.frame("package" = rep_len(pkg, length(extdata_files)))
      )
  }

  extdata_files
}

#' List system files for a package
#'
#' @noRd
list_system_files <- function(pkg,
                              dir = "extdata",
                              full.names = TRUE,
                              recursive = TRUE,
                              include.dirs = FALSE,
                              ...) {
  list.files(
    path = system.file(dir, package = pkg),
    recursive = recursive,
    full.names = full.names,
    include.dirs = include.dirs,
    ...
  )
}

#' @name list_pkg_cachedata
#' @rdname list_pkg_data
#' @export
#' @importFrom rlang is_installed is_empty
#' @importFrom rappdirs user_cache_dir
list_pkg_cachedata <- function(pkg,
                               full.names = TRUE,
                               recursive = TRUE) {
  check_string(pkg)
  cache_dir <- rappdirs::user_cache_dir(pkg)

  cache_files <- list.files(
    path = cache_dir,
    recursive = recursive,
    full.names = full.names
  )

  if (!rlang::is_empty(cache_files)) {
    cache_files <-
      cbind(
        data.frame("item" = str_remove_fileext(basename(cache_files))),
        data.frame("path" = cache_files),
        data.frame("package" = rep_len(pkg, length(cache_files)))
      )
  }

  cache_files
}
