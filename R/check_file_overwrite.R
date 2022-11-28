#' Check if a file exists and remove file or error
#'
#' @param filename File name, Default: `NULL`
#' @param path File path, Default: `NULL`
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
  filename <- filename %||% basename(path)
  filepath <- filename

  if (!is.null(path)) {
    if (has_fileext(path) && is.null(filename)) {
      filepath <- path
      path <- dirname(path)
    } else {
      filepath <- file.path(path, filename)
    }
  }

  cliExtras::cli_abort_ifnot(
    "{.arg filename} or {.arg path} must include a valid file extension.",
    condition = has_fileext(filepath),
    call = call
  )

  if (file.exists(filepath)) {
    if (!overwrite && ask && rlang::is_interactive()) {
      overwrite <-
        cliExtras::cli_yesno(
          c(
            "i" = "A file with the same name exists in {.path {path}}",
            ">" = "Do you want to overwrite {.val {filename}}?"
          )
        )
    }

    cliExtras::cli_abort_ifnot(
      c(
        "!" = "{.file {filename}} can't be saved.",
        "i" = "A file with the same name already exists.
        Set {.code overwrite = TRUE} to remove."
      ),
      condition = overwrite,
      call = call
    )

    cliExtras::cli_inform_ifnot(
      c("v" = "Removing {.path {filepath}}"),
      condition = quiet
    )

    file.remove(filepath)
  }

  invisible(NULL)
}
