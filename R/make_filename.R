#' Make file name and path with optional label, prefix, or postfix
#'
#' A helper function to create consistent file names for plots created with data
#' files.
#'
#' @param name Name to make file name converted to snake case with
#'   [janitor::make_clean_names()], e.g. "Residential zoning map" becomes
#'   "residential_zoning_map"
#' @param label Label to combine with name converted to snake case with
#'   [janitor::make_clean_names()]. The label is designed to identify the area
#'   or other shared characteristics across multiple data files, maps, or plots.
#' @param fileext File type or extension.
#' @param filename File name; if file name is `NULL`, name and file type are
#'   both required.
#' @param path Path to file or data directory.
#' @param prefix File name prefix. "date" adds a date prefix, "time" adds a
#'   date/time prefix; defaults to `NULL`.
#' @inheritParams str_pad_digits
#' @param postfix File name postfix; defaults to `NULL`.
#' @param cache If `TRUE`, path is set to the package cache directory using
#'   [get_data_dir()]; defaults to `FALSE`.
#' @inheritParams get_data_dir
#' @inheritParams str_increment_digits
#' @family read_write
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
  cliExtras::cli_abort_ifnot(
    "{.arg name} or {.arg filename} must be provided.",
    condition = is.character(c(name, filename))
  )

  # If filename is provided, remove file extension (if filename includes the
  # extension)
  if (!is.null(filename)) {
    fileext <- fileext %||% str_extract_fileext(filename)
    filename <- str_remove_fileext(filename)
  }

  # If file name is not provided, filename is based on label, name, pad and
  # width
  if (!is.null(name)) {
    cliExtras::cli_warn_ifnot(
      "The provided {.arg filename} can't be used
      if {.arg name} is also provided.",
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
