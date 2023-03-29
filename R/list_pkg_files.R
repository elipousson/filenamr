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
                          call = caller_env()) {
  rlang::check_installed(pkg, version = version, call = call)

  rbind(
    list_pkg_datasets(pkg),
    list_pkg_extdata(pkg),
    list_pkg_cachedata(pkg)
  )
}

#' @name list_pkg_datasets
#' @rdname list_pkg_data
#' @export
#' @importFrom utils data
list_pkg_datasets <- function(pkg, envir = .GlobalEnv) {
  data_files <-
    as.data.frame(
      utils::data(package = pkg, envir = envir)[["results"]]
    )
  data_files <- data_files[, c("Package", "Item", "LibPath")]
  names(data_files) <- c("package", "item", "path")
  data_files
}

#' @name list_pkg_extdata
#' @rdname list_pkg_data
#' @export
#' @importFrom rlang is_empty
list_pkg_extdata <- function(pkg,
                             full.names = TRUE,
                             recursive = TRUE) {
  extdata_dir <- system.file("extdata", package = pkg)

  extdata_files <- list.files(
    path = extdata_dir,
    recursive = recursive,
    full.names = full.names
  )

  if (!rlang::is_empty(extdata_files)) {
    extdata_files <-
      cbind(
        data.frame("package" = rep_len("mapbaltimore", length(extdata_files))),
        data.frame("path" = extdata_files),
        data.frame("item" = str_remove_fileext(basename(extdata_files)))
      )
  }

  extdata_files
}

#' @name list_pkg_cachedata
#' @rdname list_pkg_data
#' @export
#' @importFrom rlang is_installed is_empty
#' @importFrom rappdirs user_cache_dir
list_pkg_cachedata <- function(pkg,
                               full.names = TRUE,
                               recursive = TRUE) {
  cache_dir <- rappdirs::user_cache_dir(pkg)

  cache_files <- list.files(
    path = cache_dir,
    recursive = recursive,
    full.names = full.names
  )

  if (!rlang::is_empty(cache_files)) {
    cache_files <-
      cbind(
        data.frame("package" = rep_len("mapbaltimore", length(cache_files))),
        data.frame("path" = cache_files),
        data.frame("item" = str_remove_fileext(basename(cache_files)))
      )
  }

  cache_files
}
