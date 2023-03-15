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
#' @param call Passed to [cli_abort()], Default: [caller_env()]
#' @rdname check_file_overwrite
#' @export
#' @importFrom rlang caller_env is_interactive
check_file_overwrite <- function(filename = NULL,
                                 path = NULL,
                                 overwrite = TRUE,
                                 quiet = FALSE,
                                 ask = TRUE,
                                 call = caller_env()) {
  check_fileext(filename = filename, path = path, call = call)

  filepath <- filename

  if (!is.null(path)) {
    if (has_fileext(path) && is.null(filename)) {
      filename <- basename(path)
      filepath <- path
      path <- dirname(path)
    } else {
      filepath <- file.path(path, filename)
    }
  }

  if (file.exists(filepath)) {
    if (!overwrite && ask && rlang::is_interactive()) {
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

    cli_abort_ifnot(
      c(
        "!" = "{.file {filename}} can't be saved.",
        "i" = "A file with the same name already exists.
        Set {.code overwrite = TRUE} to remove."
      ),
      condition = overwrite,
      call = call
    )

    cli_inform_ifnot(
      c("v" = "Removing {.path {filename}}"),
      condition = quiet
    )

    file.remove(filepath)
  }

  invisible(NULL)
}

#' @keywords internal check
#' @noRd
check_fileext <- function(filename = NULL,
                           path = NULL,
                           fileext = NULL,
                           call = parent.frame()) {
  if (!is.null(fileext)) {
    return(invisible(NULL))
  }

  fileext_check <- c(has_fileext(filename), has_fileext(path))

  cli_abort_ifnot(
    message = "{.arg filename} or {.arg path} must include a single valid file extension.",
    condition = any(fileext_check) && !all(fileext_check),
    call = call
  )
}
