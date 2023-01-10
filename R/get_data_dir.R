#' Check if data directory exists and create a new directory if needed
#'
#' Get the path for a package-specific cache directory with
#' [rappdirs::user_cache_dir()], check for the existence of a data directory,
#' optionally create a new directory at the provided path location.
#'
#' @param path Path to directory for use as data directory.
#' @param cache If `TRUE`, and path is `NULL` set path to
#'   [rappdirs::user_cache_dir()] (using value of pkg as appname). If path is
#'   not `NULL`, the path is returned even if cache is `TRUE`.
#' @param create If `FALSE` and path does not exist, return path with a warning.
#'   If `TRUE` and [rlang::is_interactive()] is `TRUE`, ask user if directory
#'   should be created. If the session not interactive and create is `TRUE`, a
#'   new directory will be created.
#' @param appname Passed to [rappdirs::user_cache_dir()]
#' @param null.ok If `TRUE`, path is `NULL`, cache is `FALSE`, return the `NULL`
#'   path value; defaults to `TRUE`.
#' @param ask If `TRUE`, create is `FALSE`, and session is interactive, ask to
#'   create directory if the provided directory does not exist.
#' @param quiet If `TRUE`, suppress informational messages.
#' @export
#' @importFrom rlang is_interactive
get_data_dir <- function(path = NULL,
                         cache = FALSE,
                         create = TRUE,
                         ask = TRUE,
                         appname = NULL,
                         null.ok = TRUE,
                         quiet = FALSE) {
  if (quiet) {
    return(
      suppressWarnings(
        suppressMessages(
          get_data_dir(
            path,
            cache,
            create,
            appname,
            null.ok
          )
        )
      )
    )
  }

  if (cache) {
    rlang::check_installed("rappdirs")
    path <- path %||% rappdirs::user_cache_dir(appname)
  }

  if (!is.null(path) && dir.exists(path)) {
    return(path)
  }

  if (is.null(path)) {
    if (null.ok) {
      return(invisible(path))
    }

    cli_abort("{.arg path} can't be {.val NULL} when {.code null.ok = FALSE}")
  }

  if (!create) {
    cli_warn("The provided {.arg path} {.file {path}} can't be found.")
    return(path)
  }

  if (rlang::is_interactive() && !create && ask) {
    create <-
      cliExtras::cli_yesno(
        c(
          "x" = "The directory {.file {path}} does not exist.",
          ">" = "Do you want to create a directory at this location?"
        )
      )
  }

  if (!create) {
    return(invisible(NULL))
  }

  cli_inform_ifnot(
    c("v" = "New directory created at {.file {path}}"),
    condition = quiet
  )

  dir.create(path)
  invisible(path)
}

#' Get file types or extensions for files at a path
#'
#' If fileext is provided, [get_path_fileext()] will pass the file extension
#' forward without checking it.
#'
#' @param path A valid directory or file path.
#' @param fileext If not `NULL`, function returns file type as is.
#' @param n Max number of unique file types to return. Returns warning and n
#'   most common file types if path has more than n unique file types.
#' @param quiet If `TRUE`, suppress informational messages.
#' @export
get_path_fileext <- function(path,
                             fileext = NULL,
                             n = 1,
                             quiet = FALSE) {
  if (!is.null(fileext)) {
    return(fileext)
  }

  file_list <- NULL

  if (dir.exists(path)) {
    file_list <- list.files(path)
  } else if (file.exists(path)) {
    file_list <- path
  }

  cli_abort_ifnot(
    c("A valid file or directory {.arg path} must be provided.",
      "i" = "The provided {.arg path} {.file {path}} does not exist."
    ),
    condition = !is.null(file_list)
  )

  fileext <- str_extract_fileext(file_list)

  if (length(unique(fileext)) <= n) {
    return(unique(fileext))
  }

  # https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
  fileext <- names(sort(table(fileext), decreasing = TRUE)[1:n])

  cli_inform_ifnot(
    c("The directory {.file {path}} has more than {n} unique file extensions.",
      "i" = "Using {n} most common file extensions{?s}: {.val {fileext}}"
    ),
    condition = quiet
  )

  fileext
}

#' Get list of files at a path (using a single file type at a time)
#'
#' @noRd
#' @importFrom rlang has_name
get_path_files <- function(path, fileext = NULL, full.names = TRUE) {
  if (is.data.frame(path) && rlang::has_name(path, "path")) {
    path <- path[["path"]]
  }

  if (all(dir.exists(path))) {
    return(
      list.files(
        path = path,
        pattern = paste0("\\.", get_path_fileext(path, fileext), "$"),
        full.names = full.names
      )
    )
  } else if (all(file.exists(path))) {
    return(path)
  }

  cli_abort(
    c("A valid file or directory {.arg path} must be provided.",
      "i" = "The provided {.arg path} {.file {path}} does not exist."
    )
  )
}
