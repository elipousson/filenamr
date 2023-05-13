## code to prepare `default_tags` dataset goes here
default_exif <-
  c(
    "Title",
    "ImageDescription",
    "Keywords",
    "Headline",
    "Byline",
    "Caption",
    "FileName",
    "CreateDate",
    "DateTimeOriginal",
    "OffsetTimeOriginal",
    "ImageWidth",
    "ImageHeight",
    "Orientation",
    "SourceFile",
    "FileSize",
    "FileType",
    "*GPS*"
  )

default_xmpdc <-
  c(
    "XMP-dc:Title",
    "XMP-dc:Subject",
    "XMP-dc:Description"
  )

default_iptc <-
  c(
    "IPTC:Keywords",
    "IPTC:Headline",
    "IPTC:ObjectName",
    "IPTC:Caption-Abstract",
    "IPTC:AltTextAccessibility",
    "IPTC:Credit"
  )


# FIXME: The default fields likely vary by file type and could be set based
# on that. If that update is made, default tags should be converted into a
# data.frame
# NOTE: Are there other tags that should be included by default?

default_exif_tags <- c(default_exif, default_xmpdc, default_iptc)

usethis::use_data(default_exif_tags, overwrite = TRUE)

default_exif_xwalk <-
  list(
    "title" = "Title",
    "headline" = "Headline",
    "object_name" = "ObjectName",
    "file_name" = "FileName",
    "file_size" = "FileSize",
    "file_type" = "FileType",
    "img_description" = "image_description",
    "date_created" = "CreateDate",
    "date_time_original" = "DateTimeOriginal",
    "offset_time_original" = "OffsetTimeOriginal",
    "lon" = "GPSLongitude",
    "lon_ref" = "GPSLongitudeRef",
    "lat" = "GPSLatitude",
    "lat_ref" = "GPSLatitudeRef",
    "speed" = "GPSSpeed",
    "speed_ref" = "GPSSpeedRef",
    "dest_bearing" = "GPSDestBearing",
    "dest_bearing_ref" = "GPSDestBearingRef",
    "img_dir" = "GPSImgDirection",
    "img_dir_ref" = "GPSImgDirectionRef",
    "altitude" = "GPSAltitude",
    "altitude_ref" = "GPSAltitudeRef",
    "h_pos_error" = "GPSHPositioningError",
    "position" = "GPSPosition",
    "path" = "SourceFile",
    "img_width" = "ImageWidth",
    "img_height" = "ImageHeight",
    "exif_orientation" = "Orientation",
    "keywords" = "Keywords"
  )

usethis::use_data(default_exif_xwalk, overwrite = TRUE)
