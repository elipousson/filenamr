#' Make file name and path with optional label, prefix, or postfix
#'
#' A helper function to create consistent file names for plots created with data
#' files.
#'
#' @param name Name to make file name converted to snake case with
#'   [janitor::make_clean_names()], e.g. "Residential zoning map" becomes
#'   "residential_zoning_map". If the name includes a file extension it is
#'   assumed that the filename has been provided as the name parameter.
#' @param label Label to combine with name converted to snake case with
#'   [janitor::make_clean_names()]. The label is designed to identify the area
#'   or other shared characteristics across multiple data files, maps, or plots.
#'   label is ignored if name is NULL or if name includes a file extension.
#' @param fileext File type or extension. Optional if filename or path include a
#'   file extension.
#' @param filename File name; if filename is `NULL` and path does not include a
#'   file extension, name and file extension are both required.
#' @param path Path to file or data directory. Optional. If path includes a file
#'   extension and filename and fileext are both `NULL`, the filename and
#'   extension included with path will be used instead. If multiple file
#'   extensions are provided to filename, path, or fileext, `make_filename()`
#'   will abort.
#' @param prefix File name prefix. "date" adds a date prefix, "time" adds a
#'   date/time prefix; defaults to `NULL`.
#' @inheritParams isstatic::str_pad_digits
#' @param postfix File name postfix; defaults to `NULL`.
#' @param cache If `TRUE`, path is set to the package cache directory using
#'   [get_data_dir()]; defaults to `FALSE`.
#' @inheritParams get_data_dir
#' @inheritParams isstatic::str_increment_digits
#' @family read_write
#' @examples
#'
#' make_filename(
#'   filename = "image.jpeg"
#' )
#'
#' make_filename(
#'   name = "plot",
#'   label = "Group a",
#'   fileext = "png"
#' )
#'
#' make_filename(
#'   name = "plot",
#'   prefix = "date",
#'   fileext = "png"
#' )
#'
#' make_filename(
#'   name = "map_1",
#'   increment = TRUE,
#'   fileext = "geojson"
#' )
#'
#' @export
make_filename <- function(name = NULL,
                          label = NULL,
                          fileext = NULL,
                          filename = NULL,
                          path = NULL,
                          prefix = NULL,
                          postfix = NULL,
                          pad = NULL,
                          width = NULL,
                          cache = FALSE,
                          appname = NULL,
                          create = TRUE,
                          increment = NULL) {
  cli_abort_ifnot(
    "{.arg name}, {.arg filename}, or a file {.arg path} must be provided.",
    condition = is.character(c(name, filename, path))
  )

  if (has_fileext(name)) {
    filename <- name
    name <- NULL
  } else if (is.null(name) && has_fileext(path)) {
    filename <- filename %||% basename(path)
  }

  check_fileext(filename, path, fileext)

  # If filename is provided, remove file extension (if filename includes the
  # extension)
  if (!is.null(filename)) {
    fileext <- fileext %||% str_extract_fileext(filename)
    filename <- str_remove_fileext(filename)
  }

  # If file name is not provided, filename is based on label, name, pad and
  # width
  if (!is.null(name)) {
    cli_warn_ifnot(
      "{.arg filename} is ignored if a {.arg name} argument is provided.",
      condition = is.null(filename)
    )

    filename <-
      str_affix(
        prefix = label,
        string = name,
        pad = pad,
        width = width,
        use_clean_names = TRUE,
        use_make_names = FALSE
      )
  } else {
    cli_warn_ifnot(
      "{.arg label} is ignored if {.arg name} is not provided
      (or if {.arg name} contains a file extension).",
      condition = is.null(label)
    )
  }

  # Apply prefix and postfix to the filename
  filename <-
    str_affix(
      string = filename,
      prefix = prefix,
      postfix = postfix,
      pad = NULL,
      use_clean_names = TRUE,
      use_make_names = FALSE
    )

  # Append fileext
  filename <-
    str_add_fileext(
      filename,
      fileext
    )

  path <-
    get_data_dir(
      path = path,
      cache = cache,
      create = create,
      appname = appname,
      null.ok = TRUE
    )

  filename <- str_increment_digits(filename, increment = increment)

  if (is.null(path)) {
    return(filename)
  }

  file.path(path, filename)
}
