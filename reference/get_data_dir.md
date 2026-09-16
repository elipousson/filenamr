# Check if data directory exists and create a new directory if needed

Get the path for a package-specific cache directory with
[`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html),
check for the existence of a data directory, optionally create a new
directory at the provided path location.

## Usage

``` r
get_data_dir(
  path = NULL,
  cache = FALSE,
  create = TRUE,
  ask = TRUE,
  appname = NULL,
  pkg = NULL,
  allow_null = TRUE,
  quiet = FALSE,
  recursive = TRUE,
  call = caller_env()
)

list_path_filenames(
  path,
  fileext = NULL,
  pattern = NULL,
  full.names = TRUE,
  call = caller_env(),
  ...
)
```

## Arguments

- path:

  Path to directory for use as data directory.

- cache:

  If `TRUE`, and path is `NULL` set path to
  [`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html)
  (using value of pkg as appname). If path is not `NULL`, the path is
  returned even if cache is `TRUE`.

- create:

  If `FALSE` and path does not exist, return path with a warning. If
  `TRUE` and
  [`rlang::is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.html)
  is `TRUE`, ask user if directory should be created. If the session not
  interactive and create is `TRUE`, a new directory will be created.

- ask:

  If `TRUE`, create is `FALSE`, and session is interactive, ask to
  create directory if the provided directory does not exist.

- appname, pkg:

  pkg is used if appname is NULL. Passed to
  [`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html)

- allow_null:

  If `TRUE`, path is `NULL`, cache is `FALSE`, return the `NULL` path
  value; defaults to `TRUE`.

- quiet:

  If `TRUE`, suppress informational messages.

- recursive:

  logical. Should elements of the path other than the last be created?
  If true, like the Unix command `mkdir -p`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- fileext:

  File extension. If supplied to `list_path_filenames()` and pattern is
  `NULL`, only return file names matching this extension.

- pattern:

  an optional [regular expression](https://rdrr.io/r/base/regex.html).
  Only file names which match the regular expression will be returned.

- full.names:

  a logical value. If `TRUE`, the directory path is prepended to the
  file names to give a relative file path. If `FALSE`, the file names
  (rather than paths) are returned.

- ...:

  Additional parameters passed to
  [`list.files()`](https://rdrr.io/r/base/list.files.html) by
  `list_path_filenames()`.
