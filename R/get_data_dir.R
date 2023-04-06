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
#' @importFrom cliExtras cli_yesno
#' @importFrom rlang check_installed is_interactive
#' @importFrom cli cli_alert_warning cli_alert_success
#' @importFrom rappdirs user_cache_dir
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

#' @name list_path_filenames
#' @rdname get_data_dir
#' @param fileext File extension. If supplied to [list_path_filenames()] and
#'   pattern is `NULL`, only return file names matching this extension.
#' @inheritParams base::list.files
#' @param ... Additional parameters passed to [list.files()] by
#'   [list_path_filenames()].
#' @export
#' @importFrom rlang caller_env check_required has_name
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
