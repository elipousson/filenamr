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
#' @param appname,pkg pkg is used if appname is NULL. Passed to
#'   [rappdirs::user_cache_dir()]
#' @param allow_null If `TRUE`, path is `NULL`, cache is `FALSE`, return the
#'   `NULL` path value; defaults to `TRUE`.
#' @param ask If `TRUE`, create is `FALSE`, and session is interactive, ask to
#'   create directory if the provided directory does not exist.
#' @param quiet If `TRUE`, suppress informational messages.
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom cliExtras cli_quiet cli_yesno
#' @importFrom rlang check_installed is_interactive
#' @importFrom cli cli_alert_warning cli_alert_success
get_data_dir <- function(path = NULL,
                         cache = FALSE,
                         create = TRUE,
                         ask = TRUE,
                         appname = NULL,
                         pkg = NULL,
                         allow_null = TRUE,
                         quiet = FALSE,
                         call = caller_env()) {
  appname <- appname %||% pkg
  cli_quiet(quiet)

  if (cache) {
    rlang::check_installed("rappdirs")
    path <- path %||% rappdirs::user_cache_dir(appname)
  }

  if (!is.null(path) && dir.exists(path)) {
    return(path)
  }

  if (is.null(path)) {
    if (allow_null) {
      return(invisible(path))
    }

    cli_abort(
      "{.arg path} can't be {.val NULL} when {.code allow_null = FALSE}",
      call = call
    )
  }

  if (!create) {
    cli::cli_warn(
      "{.arg path} {.file {path}} can't be found."
    )
    return(path)
  }

  if (rlang::is_interactive() && !create && ask) {
    create <-
      cliExtras::cli_yesno(
        c(
          "x" = "The directory {.file {path}} can't be found.",
          ">" = "Do you want to create a directory at this location?"
        )
      )
  }

  if (!create) {
    cli::cli_alert_warning(
      "No directory created at {.file {path}}"
    )
    return(invisible(NULL))
  }

  cli::cli_alert_success(
    "New directory created at {.file {path}}"
  )

  dir.create(path)
  invisible(path)
}

#' Get file types or extensions for files at a path
#'
#' If fileext is provided, [get_path_fileext()] will pass the file extension
#' forward without checking it.
#'
#' @param path A single directory or file path. The directory or file must
#'   exist.
#' @param fileext If not `NULL`, function returns file type as is.
#' @param n Max number of unique file types to return. Returns warning and n
#'   most common file types if path has more than n unique file types.
#' @param quiet If `TRUE`, suppress informational messages.
#' @inheritParams rlang::args_error_context
#' @export
get_path_fileext <- function(path,
                             fileext = NULL,
                             n = 1,
                             quiet = FALSE,
                             call = caller_env()) {
  cli_quiet(quiet)

  if (!is.null(fileext)) {
    check_string(fileext, call = call)
    return(fileext)
  }

  fileext <- list_path_fileext(path, call = call)

  if (length(unique(fileext)) <= n) {
    return(unique(fileext))
  }

  # https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
  fileext <- str_n_freq(fileext, n)

  cli::cli_bullets(
    c("i" = "{.arg path} {.file {path}} has more than {n} unique file extension{?s}.",
      "*" = "Returning the {n} most common file extension{?s}: {.val {fileext}}"
    )
  )

  fileext
}

#' List path file extensions
#'
#' @noRd
list_path_fileext <- function(path, allow_null = FALSE, call = caller_env(), ...) {
  file_list <- NULL
  check_string(path, call = call)

  if (dir.exists(path)) {
    file_list <- list.files(path, ...)
  } else if (file.exists(path)) {
    file_list <- path
  }

  empty_file_list <- (identical(file_list, character(0)) || is.null(file_list))

  if (empty_file_list && isTRUE(allow_null)) {
    return(invisible(NULL))
  }

  cli_abort_ifnot(
    c("A valid file or directory {.arg path} must be supplied.",
      "i" = "{.arg path} {.path {path}} does not exist."
    ),
    condition = is_false(empty_file_list),
    call = call
  )

  str_extract_fileext(file_list)
}

#' Get list of files at a path (using a single file type at a time)
#'
#' @noRd
#' @importFrom rlang has_name
list_path_filenames <- function(path,
                                fileext = NULL,
                                pattern = NULL,
                                full.names = TRUE,
                                call = caller_env(),
                                ...) {
  rlang::check_required(path, call = call)
  if (is.data.frame(path) && rlang::has_name(path, "path")) {
    path <- path[["path"]]
  }

  if (all(dir.exists(path))) {
    pattern <- pattern %||% paste0("\\.", get_path_fileext(path, fileext), "$")

    return(
      list.files(
        path = path,
        pattern = pattern,
        full.names = full.names,
        ...
      )
    )
  } else if (all(file.exists(path))) {
    return(path)
  }

  cli_abort(
    c("A valid file or directory {.arg path} must be provided.",
      "i" = "The provided {.arg path} {.file {path}} does not exist."
    ),
    call = call
  )
}
