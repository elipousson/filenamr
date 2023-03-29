#' Get file types or extensions for files at a path
#'
#' If fileext is provided, [get_path_fileext()] will pass the file extension
#' forward without checking it. [list_path_fileext()] is a more basic function
#' that list file extensions at a path directory.
#'
#' @param path A single directory or file path. The directory or file must
#'   exist.
#' @param fileext If not `NULL`, function returns file type as is.
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
