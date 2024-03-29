#' Read EXIF metadata to create a simple feature object or write
#' EXIF metadata to image files
#'
#' @description
#' [read_exif()] read EXIF data from folder of files. Optionally assigns
#' a cardinal direction based on the direction metadata and recodes the
#' orientation metadata. Note that tags must include GPS tags if you plan to
#' create an `sf` object based on the resulting data.frame object.
#'
#' For [write_exif()] the parameters are used to multiple tags with the same
#' values:
#'
#' - title: Title, IPTC:Headline, IPTC:ObjectName, XMP-dc:Title
#' - description: ImageDescription, XMP-dc:Description, and
#' IPTC:Caption-Abstract
#' - keywords: Keywords, IPTC:Keywords, XMP-dc:Subject
#'
#' @param path A path to folder or file.
#' @param fileext The file extension or file type; defaults to `NULL`.
#' @param tags List of EXIF tags to read from files. If
#'   `NULL` (default), set to option "filenamr.exif_tags" or default
#'   `default_exif_tags`.
#' @param format_exif If `TRUE` (default), rename columns based on xwalk values,
#'   add cardinal directions based on bearing, and format date columns.
#' @param xwalk If `NULL`, set to option "filenamr.exif_xwalk" or default
#'   `default_exif_xwalk`.
#' @param tz Time zone to pass to [lubridate::ymd_hms()] if format_exif is
#'   `TRUE`. Typically set to `Sys.timezone()` to convert date/time columns.
#' @inheritParams tibble::as_tibble
#' @param ... Additional parameters to pass to [exiftoolr::exif_read()]
#' @returns A tibble of EXIF and other metadata from files located in the path
#'   directory.
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang has_name
read_exif <- function(path = NULL,
                      fileext = NULL,
                      tags = NULL,
                      format_exif = TRUE,
                      xwalk = NULL,
                      tz = NULL,
                      .name_repair = "check_unique",
                      ...) {
  check_installed("exiftoolr")

  filenames <- list_path_filenames(path, fileext)

  if (has_length(filenames, 0)) {
    text <- "No files found at {.arg path}: {.path {path}}"
    if (!is.null(fileext)) {
      text <- "No {.val {fileext}} files found at {.arg path}: {.path {path}}"
    }
    cli::cli_alert_warning(text, wrap = TRUE)
    return(invisible(NULL))
  }

  tags <- tags %||% getOption("filenamr.exif_tags", default = default_exif_tags)

  # FIXME: This is a partial list of filetypes that support GPS EXIF metadata
  # fileext <- match.arg(fileext, c("jpg", "jpeg", "png", "tiff", "pdf"))

  # FIXME: Figure out how to append path to the end of the table not the
  # beginning
  data <-
    exiftoolr::exif_read(
      path = filenames,
      tags = tags,
      ...
    )

  check_installed(c("tibble", "dplyr", "lubridate"))

  data <-
    tibble::as_tibble(
      data,
      .name_repair = .name_repair
    )

  if (!format_exif) {
    return(data)
  }

  xwalk <- xwalk %||%
    getOption("filenamr.exif_xwalk", default = default_exif_xwalk)

  xwalk <- xwalk[has_name(data, xwalk)]

  data <-
    # Rename variables
    dplyr::rename_with(
      data,
      ~ names(xwalk)[which(xwalk == .x)],
      .cols = dplyr::all_of(as.character(xwalk))
    )

  if (!is_null(tz)) {
    data <-
      dplyr::mutate(
        data,
        dplyr::across(
          dplyr::contains(c("Date", "date")),
          function(x) {
            lubridate::ymd_hms(x, tz = tz)
          }
        )
      )
  }

  data <- fmt_exif_orientation(data)

  data <- fmt_exif_direction(data)

  data
}


#' @name write_exif
#' @rdname read_exif
#' @param title Title to add to file metadata with exiftoolr, Default: `NULL`.
#' @param author Author to add to file metadata to the "Author" and
#'   "XMP-dc:creator" tags. Default: `NULL`.
#' @param credit Credit to add to file metadata to the "IPTC:Credit" and
#'   "XMP-dc:Credit" tags. Defaults to the same value as author.
#' @param date Date to add to file metadata with exiftoolr (not currently
#'   working).  Defaults to `NULL`.
#' @param keywords Keyword(s) added to file metadata to "IPTC:Keywords" and
#'   "XMP-dc:Subject" tags. Defaults to `NULL`.
#' @param description Description added to the "ImageDescription",
#'   "IPTC:Caption-Abstract", and "XMP-dc:Description" tags.
#' @param alt Text to pass as alt text to the "IPTC:AltTextAccessibility" and
#'   "iTXt" (PNG files only) tags. Defaults to `NULL`.
#' @param metadata Any of the other metadata parameters (title, author, credit,
#'   date, keywords, description, and alt) can also be set by passing a named
#'   list or data.frame to metadata. If an argument is supplied, any conflicting
#'   value in metadata is ignored.
#' @param args Alternate arguments passed to [exiftoolr::exif_call()]. Other tag
#'   parameters are appended to args if they are not `NULL`.
#' @param overwrite If `TRUE`, overwrite any existing EXIF metadata present in
#'   the provided fields; defaults to `TRUE`
#' @param append_keywords If `TRUE`, append keywords, if `FALSE`, replace
#'   keywords in file metadata.
#' @param quiet If `TRUE` (default), suppress function messages.
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom rlang check_installed
#' @importFrom cliExtras cli_list_files
write_exif <- function(path,
                       fileext = NULL,
                       title = NULL,
                       author = NULL,
                       credit = author,
                       date = NULL,
                       keywords = NULL,
                       description = NULL,
                       alt = NULL,
                       metadata = NULL,
                       args = NULL,
                       overwrite = TRUE,
                       append_keywords = FALSE,
                       quiet = FALSE,
                       call = caller_env()) {
  check_installed("exiftoolr")
  cli_quiet(quiet)

  args <-
    set_write_exif_args(
      title = title,
      author = author,
      credit = credit,
      date = date,
      keywords = keywords,
      description = description,
      alt = alt,
      metadata = metadata,
      args = args,
      overwrite = overwrite,
      append_keywords = append_keywords,
      fileext = fileext,
      call = call
    )

  if (all(dir.exists(path))) {
    files <- list_path_filenames(path, fileext = fileext)
  } else {
    if (!all(is_file(path))) {
      cli::cli_abort(
        "{.arg path} must be an existing directory or character vector of existing files.",
        call = call
      )
    }

    files <- path
  }

  suppressMessages(
    suppressWarnings(
      exiftoolr::exif_call(
        args = args,
        path = path,
        quiet = TRUE
      )
    )
  )

  cliExtras::cli_list_files(
    files = files,
    text = c("v" = "Updated EXIF metadata for {length(files)} file{?s}:")
  )
}


#' Pass file path and replacement tag values to write_exif based on selected tag
#'
#' @noRd
walk2_write_exif <- function(path,
                             replacement_vals,
                             tag = "keywords") {
  walk_vars <- set_names(replacement_vals, path)

  if (tag == "keywords") {
    walk(
      seq_along(walk_vars),
      function(i) {
        write_exif(
          path = names(walk_vars)[i], keywords = walk_vars[[i]],
          overwrite = TRUE,
          append_keywords = FALSE
        )
      }
    )
  }

  if (tag == "title") {
    walk(
      seq_along(walk_vars),
      function(i) {
        write_exif(
          path = names(walk_vars)[i], title = walk_vars[[i]],
          overwrite = TRUE
        )
      }
    )
  }

  if (tag == "description") {
    walk(
      seq_along(walk_vars),
      function(i) {
        write_exif(
          path = names(walk_vars)[i], description = walk_vars[[i]],
          overwrite = TRUE
        )
      }
    )
  }
}

#' @noRd
set_write_exif_args <- function(title = NULL,
                                author = NULL,
                                credit = author,
                                date = NULL,
                                keywords = NULL,
                                description = NULL,
                                alt = NULL,
                                metadata = NULL,
                                args = NULL,
                                overwrite = TRUE,
                                append_keywords = FALSE,
                                fileext = NULL,
                                call = caller_env()) {
  if (is.list(metadata)) {
    title <- title %||% metadata[["title"]]
    author <- author %||% metadata[["author"]]
    credit <- credit %||% metadata[["credit"]]
    keywords <- keywords %||% metadata[["keywords"]]
    description <- description %||% metadata[["description"]]
    alt <- alt %||% metadata[["alt"]]
  }

  # FIXME: I want to implement a method that allows adding, replacing, or
  # modifying exif
  if (!is.null(title)) {
    args <- c(args, glue("-Title={title}"))
    args <- c(args, glue("-IPTC:Headline={title}"))
    args <- c(args, glue("-IPTC:ObjectName={title}"))
    args <- c(args, glue("-XMP-dc:Title={title}"))
  }

  if (!is.null(author)) {
    args <- c(args, glue("-Author={author}"))
    # args <- c(args, glue("-IPTC:Creator={author}"))
    args <- c(args, glue("-XMP-dc:creator={author}"))
  }

  if (!is.null(credit)) {
    args <- c(args, glue("-IPTC:Credit={credit}"))
    args <- c(args, glue("-XMP-dc:Credit={credit}"))
  }

  if (!is.null(description)) {
    args <- c(args, glue("-ImageDescription={description}"))
    args <- c(args, glue("-IPTC:Caption-Abstract={description}"))
    args <- c(args, glue("-XMP-dc:Description={description}"))
  }

  if (!is.null(alt)) {
    # https://exiftool.org/TagNames/IPTC.html
    # https://www.iptc.org/std/photometadata/specification/IPTC-PhotoMetadata#alt-text-accessibility
    args <- c(args, glue("-IPTC:AltTextAccessibility={alt}"))
    if ("png" %in% fileext) {
      # https://exiftool.org/TagNames/PNG.html
      args <- c(args, glue("-iTXt={alt}"))
    }
  }

  if (!is.null(date)) {
    # FIXME: exiftoolr::exif_call() does not support the "now" value supported
    # by exif If CreateDate is set to now automatically, why bother revising
    # with exiftoolr anyway?
    date <- "now"
    if ("png" %in% fileext) {
      args <- c(args, glue("-CreationTime={date}"))
    } else {
      args <- c(args, c(glue("-CreateDate={date}"), glue("-ModifyDate={date}")))
    }
  }

  if (!is.null(keywords)) {
    # TODO: Add support for subjects (partially complete with keywords)
    # https://stackoverflow.com/questions/28588696/python-exiftool-combining-subject-and-keyword-tags#28609886
    op <- "+="
    if (overwrite && !append_keywords) {
      op <- "="
    }

    # args <- c(args, paste0("-Keywords", op, keywords))
    args <- c(args, paste0("-IPTC:Keywords", op, keywords))
    args <- c(args, paste0("-XMP-dc:Subject", op, keywords))
  }

  if (is.null(args)) {
    add_args <- c(
      "title", "author", "creator", "description",
      "alt", "date", "keywords", "metadata"
    )

    cli::cli_abort(
      "{.arg args} must be supplied if {.arg {add_args}} are all {.code NULL}.",
      call = call
    )
  }

  if (overwrite) {
    args <- c(args, "-overwrite_original")
  }

  args
}
