#' Modify strings to help make consistent file names
#'
#' Functions include:
#'
#' - [str_add_fileext()]: Add file type to string
#' - [str_remove_fileext()]: Remove file type from string
#' - [str_extract_fileext()]: Extract file type from string
#' - [has_fileext()]: Does a string have a file extension (optionally
#' user-defined)?
#'
#' @name str_fileext
#' @param string Character vector
NULL

#' @name str_add_fileext
#' @rdname str_fileext
#' @param fileext File extension string
#' @export
str_add_fileext <- function(string, fileext = NULL) {
  if (!is.null(fileext) && has_fileext(string, fileext)) {
    return(string)
  }

  if (has_fileext(string)) {
    string <- str_remove_fileext(string)
  }

  # FIXME: Previously used paste0(string, ".", fileext)) -
  # make sure the change doesn't cause any new issues
  paste0(c(string, fileext), collapse = ".")
}

#' @name str_remove_fileext
#' @rdname str_fileext
#' @export
str_remove_fileext <- function(string, fileext = NULL) {
  fileext <- fileext %||% str_extract_fileext(string)
  str_remove(string, paste0("\\.", fileext, "$"))
}

#' @name str_extract_fileext
#' @rdname str_fileext
#' @param tocase Function to use with [str_extract_fileext] for case conversion
#'   of returned file extension Defaults to [tolower()].
#' @export
str_extract_fileext <- function(string, fileext = NULL, tocase = tolower) {
  fileext <- fileext %||% "[a-zA-Z0-9]+"
  tocase(
    regmatches(
      string,
      regexpr(
        paste0("(?<=\\.)", fileext, "$(?!\\.)"),
        string,
        perl = TRUE
      )
    )
  )
}
