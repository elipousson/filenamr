#' Get file extensions for files at a path
#'
#' If fileext is provided, [get_path_fileext()] will pass the file extension
#' forward without checking it. [list_path_fileext()] is a more basic function
#' that list file extensions at a path directory.
#'
#' @param path A single directory or file path. The directory or file must
#'   exist.
#' @param fileext If fileext is supplied, the function returns the file
#'   extension as is. If `NULL` (default), one or more file extensions are
#'   extracted from files at the path location.
#' @param n Max number of unique file types to return. Returns warning and n
#'   most common file types if path has more than n unique file types.
#' @param quiet If `TRUE`, suppress informational messages.
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom cli cli_bullets
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

  if (n == 1) {
    n <- "single"
  }

  cli::cli_bullets(
    c(
      "i" = "{.arg path} {.file {path}} has more than {n} unique file extension{?s}.",
      "*" = "Returning the {n} most common file extension{?s}: {.val {fileext}}"
    )
  )

  fileext
}

#' @name list_path_fileext
#' @rdname get_path_fileext
#' @param allow_null If `TRUE`, [list_path_fileext()] returns `NULL` if path is
#'   `NULL` or if no files exist at the path location. If `FALSE` (default),
#'   abort if either condition is met.
#' @param ... Additional parameters passed by [list_path_fileext()] to
#'   [list.files()].
#' @export
list_path_fileext <- function(path,
                              allow_null = FALSE,
                              call = caller_env(),
                              ...) {
  file_list <- NULL
  check_string(path, call = call)

  if (dir.exists(path)) {
    file_list <- list.files(path, ...)
  } else if (file.exists(path)) {
    file_list <- path
  }

  if (rlang::is_empty(file_list) && isTRUE(allow_null)) {
    return(invisible(NULL))
  }

  cli_if(
    x = is.null(file_list),
    c("A valid file or directory {.arg path} must be supplied.",
      "i" = "{.arg path} {.path {path}} does not exist."
    ),
    .fn = cli::cli_abort,
    call = call
  )

  cli_if(
    x = identical(file_list, character(0)),
    "Files can't be found at {.arg path} {.path {path}}",
    .fn = cli::cli_abort,
    call = call
  )

  str_extract_fileext(file_list)
}
