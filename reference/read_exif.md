# Read EXIF metadata to create a simple feature object or write EXIF metadata to image files

`read_exif()` read EXIF data from folder of files. Optionally assigns a
cardinal direction based on the direction metadata and recodes the
orientation metadata. Note that tags must include GPS tags if you plan
to create an `sf` object based on the resulting data.frame object.

For `write_exif()` the parameters are used to multiple tags with the
same values:

- title: Title, IPTC:Headline, IPTC:ObjectName, XMP-dc:Title

- description: ImageDescription, XMP-dc:Description, and
  IPTC:Caption-Abstract

- keywords: Keywords, IPTC:Keywords, XMP-dc:Subject

## Usage

``` r
read_exif(
  path = NULL,
  fileext = NULL,
  tags = NULL,
  format_exif = TRUE,
  xwalk = NULL,
  tz = NULL,
  .name_repair = "check_unique",
  ...
)

write_exif(
  path,
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
  call = caller_env()
)
```

## Arguments

- path:

  A path to folder or file.

- fileext:

  The file extension or file type; defaults to `NULL`.

- tags:

  List of EXIF tags to read from files. If `NULL` (default), set to
  option "filenamr.exif_tags" or default `default_exif_tags`.

- format_exif:

  If `TRUE` (default), rename columns based on xwalk values, add
  cardinal directions based on bearing, and format date columns.

- xwalk:

  If `NULL`, set to option "filenamr.exif_xwalk" or default
  `default_exif_xwalk`.

- tz:

  Time zone to pass to
  [`lubridate::ymd_hms()`](https://lubridate.tidyverse.org/reference/ymd_hms.html)
  if format_exif is `TRUE`. Typically set to
  [`Sys.timezone()`](https://rdrr.io/r/base/timezones.html) to convert
  date/time columns.

- .name_repair:

  Treatment of problematic column names:

  - `"minimal"`: No name repair or checks, beyond basic existence,

  - `"unique"`: Make sure names are unique and not empty,

  - `"check_unique"`: (default value), no name repair, but check they
    are `unique`,

  - `"universal"`: Make the names `unique` and syntactic

  - `"unique_quiet"`: Same as `"unique"`, but "quiet"

  - `"universal_quiet"`: Same as `"universal"`, but "quiet"

  - a function: apply custom name repair (e.g.,
    `.name_repair = make.names` for names in the style of base R).

  - A purrr-style anonymous function, see
    [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html)

  This argument is passed on as `repair` to
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html).
  See there for more details on these terms and the strategies used to
  enforce them.

- ...:

  Additional parameters to pass to
  [`exiftoolr::exif_read()`](https://joshobrien.github.io/exiftoolr/reference/exif_read.html)

- title:

  Title to add to file metadata with exiftoolr, Default: `NULL`.

- author:

  Author to add to file metadata to the "Author" and "XMP-dc:creator"
  tags. Default: `NULL`.

- credit:

  Credit to add to file metadata to the "IPTC:Credit" and
  "XMP-dc:Credit" tags. Defaults to the same value as author.

- date:

  Date to add to file metadata with exiftoolr (not currently working).
  Defaults to `NULL`.

- keywords:

  Keyword(s) added to file metadata to "IPTC:Keywords" and
  "XMP-dc:Subject" tags. Defaults to `NULL`.

- description:

  Description added to the "ImageDescription", "IPTC:Caption-Abstract",
  and "XMP-dc:Description" tags.

- alt:

  Text to pass as alt text to the "IPTC:AltTextAccessibility" and "iTXt"
  (PNG files only) tags. Defaults to `NULL`.

- metadata:

  Any of the other metadata parameters (title, author, credit, date,
  keywords, description, and alt) can also be set by passing a named
  list or data.frame to metadata. If an argument is supplied, any
  conflicting value in metadata is ignored.

- args:

  Alternate arguments passed to
  [`exiftoolr::exif_call()`](https://joshobrien.github.io/exiftoolr/reference/exif_call.html).
  Other tag parameters are appended to args if they are not `NULL`.

- overwrite:

  If `TRUE`, overwrite any existing EXIF metadata present in the
  provided fields; defaults to `TRUE`

- append_keywords:

  If `TRUE`, append keywords, if `FALSE`, replace keywords in file
  metadata.

- quiet:

  If `TRUE` (default), suppress function messages.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A tibble of EXIF and other metadata from files located in the path
directory.
