#' Set a file path and validate path file extension
#'
#' This is a flexible wrapper for [isstatic::file_path()] that allows you to
#' provide a file path as a filename or path parameter. If the path contains a
#' file extension and the fileext parameter is provided, the function aborts if
#' the two file extensions do not match.
#'
#' @param filename File name. Optional if path is supplied.
#' @param path File path. Optional if filename is supplied.
#' @param fileext File extension. If the path supplied using filename and path
#'   *does not* end with a file extension, filext is used
#'   as the file extension for the returned path. If the path, *does* end with a
#'   file extension, and fileext is used to validate the supplied.
#' @param allow_null If `TRUE`, return `NULL` if filename and path are `NULL`.
#'   If `FALSE`, error if filename and path are both `NULL`.
#' @export
#' @importFrom cli cli_alert_danger
#' @importFrom rlang is_false is_null
set_file_path <- function(filename = NULL,
                          path = NULL,
                          fileext = NULL,
                          allow_null = FALSE,
                          call = parent.frame()) {
  check_string(filename, allow_null = TRUE, call = call)
  check_string(path, allow_null = TRUE, call = call)

  if (has_fileext(filename) && has_fileext(path)) {
    cli::cli_alert_danger(
      "{.arg filename} and {.arg path} both appear to end in file extensions and
      may not make a valid file path."
    )
  }

  if (is.null(filename) && is.null(path) && rlang::is_false(allow_null)) {
    cli_abort(
      "{.arg filename} and {.arg path}  can't both be `NULL` if
      {.code allow_null = FALSE}.",
      call = call
    )
  }

  path <- file_path(path, filename, call = call, allow_null = allow_null)

  if ((is_null(path) && allow_null) || is_null(fileext)) {
    return(path)
  }

  if (!has_fileext(path)) {
    return(str_add_fileext(path, fileext))
  }

  check_path_fileext(
    path,
    fileext,
    "{.arg filename} or {.arg path} must use a {.val {fileext}}
    file extension, not {.val {str_extract_fileext(path)}}.",
    call = call
  )

  path
}
