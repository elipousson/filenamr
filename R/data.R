#' Default EXIF, XMP-dc, and IPTC tags
#'
#' A vector of default EXIF, XMP-dc, and IPTC tags (including wildcard EXIF tag
#' "GPS") used by [read_exif()].
#'
#' @format A length 26 character vector.
"default_exif_tags"

#' EXIF data column name crosswalk
#'
#' A named vector with a crosswalk of default column names returned by
#' [exiftoolr::exif_read()] and the replacement values used as new names.
#'
#' @format A length 29 named character vector.
"default_exif_xwalk"
