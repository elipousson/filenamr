#' Check if a file exists and remove file or error
#'
#' The filename or path must include a single file extension.
#'
#' @param filename File name, Default: `NULL`. Optional if path is supplied.
#' @param path File path, Default: `NULL`. Optional if filename is supplied.
#' @param overwrite If `TRUE`, remove a file with the same name and path
#' @param quiet If `TRUE`, suppress informational messages, Default: `FALSE`
#' @param ask If `TRUE`, overwrite is `FALSE`, and session is interactive, ask
#'   if user wants to overwrite the file. Default: `TRUE`
#' @inheritParams rlang::args_error_context
#' @rdname check_file_overwrite
#' @export
#' @importFrom rlang caller_env is_interactive
check_file_overwrite <- function(filename = NULL,
                                 path = NULL,
                                 overwrite = TRUE,
                                 quiet = FALSE,
                                 ask = TRUE,
                                 call = caller_env()) {
  cli_quiet(quiet)
  filepath <- set_file_path(filename, path)

  if (!is.null(path)) {
    if (has_fileext(path) && is.null(filename)) {
      filename <- basename(path)
      filepath <- path
      path <- path_dir(path)
    } else {
      filepath <- file.path(path, filename)
    }
  }

  if (file.exists(filepath)) {
    if (!overwrite && ask && is_interactive()) {
      path <- dirname(filepath)
      overwrite <-
        cliExtras::cli_yesno(
          c(
            "i" = "A file with the same name exists at {.path {path}}",
            ">" = "Do you want to overwrite {.val {filename}}?"
          ),
          n_yes = 1, n_no = 1
        )
    }

    cli_ifnot(
      overwrite,
      text = c(
        "!" = "{.file {filename}} can't be saved.",
        "i" = "A file with the same name already exists.
        Set {.code overwrite = TRUE} to remove."
      ),
      .fn = cli::cli_bullets
    )

    cli::cli_alert_success(
      "Removing {.path {filename}}"
    )

    file.remove(filepath)
  }

  invisible(NULL)
}

#' Check if a file path has a file extension
#'
#' @param path File path to check. Required.
#' @param fileext Optional file extension. If `NULL`, path must have a file
#'   extension. If a character string, path must have a matching file extension.
#' @inheritParams cliExtras::cli_abort_ifnot
#' @export
check_path_fileext <- function(path,
                               fileext = NULL,
                               message = "{.arg path} must include a file extension.",
                               call = caller_env()) {
  check_character(path, call = call)
  check_string(fileext, allow_null = TRUE, call = call)
  cli_abort_ifnot(
    message = message,
    condition = all(has_fileext(path, fileext)),
    call = call
  )
}
