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
    "IPTC:Caption-Abstract"
  )


# FIXME: The default fields likely vary by file type and could be set based
# on that. If that update is made, default tags should be converted into a
# data.frame
# NOTE: Are there other tags that should be included by default?

default_tags <- c(default_exif, default_xmpdc, default_iptc)

usethis::use_data(default_tags, overwrite = TRUE)
