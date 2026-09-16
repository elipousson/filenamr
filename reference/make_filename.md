# Make file name and path with optional label, prefix, or postfix

A helper function to create consistent file names for plots created with
data files.

## Usage

``` r
make_filename(
  name = NULL,
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
  pkg = NULL,
  create = TRUE,
  increment = NULL,
  call = caller_env()
)
```

## Arguments

- name:

  Name to make file name converted to snake case with
  [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html),
  e.g. "Residential zoning map" becomes "residential_zoning_map". If the
  name includes a file extension it is assumed that the filename has
  been provided as the name parameter.

- label:

  Label to combine with name converted to snake case with
  [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html).
  The label is designed to identify the area or other shared
  characteristics across multiple data files, maps, or plots. label is
  ignored if name is NULL or if name includes a file extension.

- fileext:

  File type or extension. Optional if filename or path include a file
  extension.

- filename:

  File name; if filename is `NULL` and path does not include a file
  extension, name and file extension are both required.

- path:

  Path to file or data directory. Optional. If path includes a file
  extension and filename and fileext are both `NULL`, the filename and
  extension included with path will be used instead. If multiple file
  extensions are provided to filename, path, or fileext,
  `make_filename()` will abort.

- prefix:

  File name prefix. "date" adds a date prefix, "time" adds a date/time
  prefix; defaults to `NULL`.

- postfix:

  File name postfix; defaults to `NULL`.

- pad:

  Single padding character added to digits in string; defaults to "0"

- width:

  Minimum width of padded strings.

- cache:

  If `TRUE`, path is set to the package cache directory using
  [`get_data_dir()`](https://elipousson.github.io/filenamr/reference/get_data_dir.md);
  defaults to `FALSE`.

- appname, pkg:

  pkg is used if appname is NULL. Passed to
  [`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html)

- create:

  If `FALSE` and path does not exist, return path with a warning. If
  `TRUE` and
  [`rlang::is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.html)
  is `TRUE`, ask user if directory should be created. If the session not
  interactive and create is `TRUE`, a new directory will be created.

- increment:

  If `TRUE`, increment digits in string by 1. If numeric, increment
  digits in string by value. If `NULL`, 0, or if no digits are present
  in string, return string as is.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Examples

``` r

make_filename(
  filename = "image.jpeg"
)
#> [1] "image.jpeg"

make_filename(
  name = "plot",
  label = "Group a",
  fileext = "png"
)
#> [1] "group_a_plot.png"

make_filename(
  name = "plot",
  prefix = "date",
  fileext = "png"
)
#> [1] "2026-09-16_plot.png"

make_filename(
  name = "map_1",
  increment = TRUE,
  fileext = "geojson"
)
#> [1] "map_2.geojson"
```
