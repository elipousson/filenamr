exif_xwalk <-
  list(
    "img_description" = "image_description",
    "lon" = "longitude",
    "lat" = "latitude",
    "lon_ref" = "longitude_ref",
    "lat_ref" = "latitude_ref",
    "path" = "source_file",
    "img_width" = "image_width",
    "img_height" = "image_height",
    "exif_orientation" = "orientation"
  )

#' Read EXIF metadata to create a simple feature object or write
#' EXIF metadata to image files
#'
#' @description
#' [read_exif()] read EXIF data from folder of files. This function also assigns
#' a cardinal direction based on the direction metadata and recodes the
#' orientation metadata.
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
#' @param bbox Optional bounding box to crop returned file (excluding images
#'   with location data outside the bounding box). If bbox is provided the
#'   returned data will match the crs of the bbox.
#' @param fileext The file extension or file type; defaults to `NULL`.
#' @param tags Optional list of EXIF tags to read from files. Must include GPS
#'   tags to create an `sf` object.
#' @param geometry If `TRUE`, convert the data.frame with coordinates to a sf
#'   object using [sf::st_as_sf()].
#' @param ... Additional EXIF tags to pass to [exiftoolr::exif_read()]
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang has_name
read_exif <- function(path = NULL,
                      fileext = NULL,
                      bbox = NULL,
                      tags = getOption("read_exif.tags", default = default_tags),
                      geometry = FALSE,
                      ...) {
  rlang::check_installed("exiftoolr")

  filenames <- list_path_filenames(path, fileext)

  if (length(filenames) == 0) {
    text <- "No files found at {.arg path}: {.path {path}}"
    if (!is.null(fileext)) {
      text <- "No {.val {fileext}} files found at {.arg path}: {.path {path}}"
    }
    cli::cli_alert_warning(text, wrap = TRUE)
    return(invisible(NULL))
  }

  # FIXME: This is a partial list of filetypes that support GPS EXIF metadata
  # fileext <- match.arg(fileext, c("jpg", "jpeg", "png", "tiff", "pdf"))
  geo_tags <- c("GPSLatitude", "GPSLongitude")

  if (geometry && !all(geo_tags %in% tags)) {
    cli_warn(
      c("{.arg tags} must be include {.val {c('GPSLatitude', 'GPSLongitude')}}
        to create a {.cls sf} object from EXIF metadata.",
        "*" = "Adding required tags to {.arg tags}."
      )
    )

    tags <- c(tags, geo_tags)
  }

  # FIXME: Figure out how to append path to the end of the table not the
  # beginning
  data <-
    suppressMessages(
      exiftoolr::exif_read(
        filenames,
        tags = tags
      )
    )

  fmt_exif_data(data)
}

#' @noRd
fmt_exif_data <- function(data, geometry = FALSE) {
  rlang::check_installed("dplyr")
  rlang::check_installed("janitor")

  data <-
    # Rename variables
    # FIXME: Is it possible to move this to the exif_xwalk?
    dplyr::rename_with(
      janitor::clean_names(data),
      ~ sub("^gps_", "", .x)
    )

  xwalk <- exif_xwalk[rlang::has_name(data, exif_xwalk)]

  data <-
    # Rename variables
    dplyr::rename_with(
      data,
      ~ names(xwalk)[which(xwalk == .x)],
      .cols = dplyr::all_of(as.character(xwalk))
    )

  data <- fmt_exif_orientation(data)

  data <- fmt_exif_direction(data)

  if (geometry) {
    rlang::check_installed("sf")

    data <-
      sf::st_as_sf(
        data,
        coords = c("lon", "lat"),
        crs = 4326
      )
  }

  data
}


#' @name write_exif
#' @rdname read_exif
#' @param title Title to add to file metadata with exiftoolr, Default: `NULL`.
#' @param author Author to add to file metadata with exiftoolr, Default: `NULL`.
#' @param date Date to add to file metadata with exiftoolr (not currently
#'   working), Default: `NULL`.
#' @param keywords Keyword(s) added to file metadata with with exiftoolr,
#'   Default: `NULL`.
#' @param description Description added to file metadata.
#' @param args Alternate arguments passed to [exiftoolr::exif_call()]. If args
#'   is not `NULL`, title, author, date, and keywords are ignored; defaults to
#'   `NULL`.
#' @param overwrite If `TRUE`, overwrite any existing EXIF metadata present in
#'   the provided fields; defaults to `TRUE`
#' @param append_keywords If `TRUE`, append keywords, if `FALSE`, replace
#'   keywords in file metadata.
#' @export
write_exif <- function(path,
                       fileext = NULL,
                       title = NULL,
                       author = NULL,
                       date = NULL,
                       keywords = NULL,
                       description = NULL,
                       # metadata = NULL,
                       args = NULL,
                       overwrite = TRUE,
                       append_keywords = FALSE) {
  rlang::check_installed("exiftoolr")

  # if (!is.null(metadata) && is.data.frame(metadata)) {
  #
  # }

  # FIXME: I want to implement a method that allows adding, replacing, or
  # modifying exif
  if (is.null(args)) {
    if (!is.null(title)) {
      args <- c(args, glue("-Title={title}"))
      args <- c(args, glue("-IPTC:Headline={title}"))
      args <- c(args, glue("-IPTC:ObjectName={title}"))
      args <- c(args, glue("-XMP-dc:Title={title}"))
    }

    if (!is.null(author)) {
      args <- c(args, glue("-Author={author}"))
    }

    if (!is.null(description)) {
      args <- c(args, glue("-ImageDescription={description}"))
      args <- c(args, glue("-IPTC:Caption-Abstract={description}"))
      args <- c(args, glue("-XMP-dc:Description={description}"))
    }

    if (!is.null(date)) {
      # FIXME: exiftoolr::exif_call() does not support the "now" value supported
      # by exif If CreateDate is set to now automatically, why bother revising
      # with exiftoolr anyway? TODO: Add support for subjects (partially
      # complete with keywords)
      # https://stackoverflow.com/questions/28588696/python-exiftool-combining-subject-and-keyword-tags#28609886
      date <- "now"
      if ("png" %in% fileext) {
        args <- c(args, glue("-CreationTime={date}"))
      } else {
        args <- c(args, c(glue("-CreateDate={date}"), glue("-ModifyDate={date}")))
      }
    }

    if (!is.null(keywords)) {
      op <- "+="

      if (overwrite && !append_keywords) {
        op <- "="
      }

      args <- c(args, paste0("-Keywords", op, keywords))
      args <- c(args, paste0("-IPTC:Keywords", op, keywords))
      args <- c(args, paste0("-XMP-dc:Subject", op, keywords))
    }

    if (overwrite) {
      args <- c(args, "-overwrite_original")
    }
  }

  if (!is.null(args)) {
    path <- list_path_filenames(path)

    suppressMessages(
      suppressWarnings(
        exiftoolr::exif_call(
          args = args,
          path = path,
          quiet = TRUE
        )
      )
    )

    cliExtras::cli_paths(path, "Updated EXIF metadata for")
  }
}


#' Pass file path and replacement tag values to write_exif based on selected tag
#'
#' @noRd
walk2_write_exif <- function(path, replacement_vals, tag = "keywords") {
  rlang::check_installed("purrr")

  if (tag == "keywords") {
    purrr::walk2(
      path,
      replacement_vals,
      ~ write_exif(
        path = .x, keywords = .y,
        overwrite = TRUE, append_keywords = FALSE
      )
    )
  } else if (tag == "title") {
    purrr::walk2(
      path,
      replacement_vals,
      ~ write_exif(
        path = .x, title = .y,
        overwrite = TRUE
      )
    )
  } else if (tag == "description") {
    purrr::walk2(
      path,
      replacement_vals,
      ~ write_exif(
        path = .x, description = .y,
        overwrite = TRUE
      )
    )
  }
}
