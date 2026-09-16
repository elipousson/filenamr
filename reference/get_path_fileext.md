# Get file extensions for files at a path

If fileext is provided, `get_path_fileext()` will pass the file
extension forward without checking it. `list_path_fileext()` is a more
basic function that list file extensions at a path directory.

## Usage

``` r
get_path_fileext(
  path,
  fileext = NULL,
  n = 1,
  quiet = FALSE,
  call = caller_env()
)

list_path_fileext(path, allow_null = FALSE, call = caller_env(), ...)
```

## Arguments

- path:

  A single directory or file path. The directory or file must exist.

- fileext:

  If fileext is supplied, the function returns the file extension as is.
  If `NULL` (default), one or more file extensions are extracted from
  files at the path location.

- n:

  Max number of unique file types to return. Returns warning and n most
  common file types if path has more than n unique file types.

- quiet:

  If `TRUE`, suppress informational messages.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- allow_null:

  If `TRUE`, `list_path_fileext()` returns `NULL` if path is `NULL` or
  if no files exist at the path location. If `FALSE` (default), abort if
  either condition is met.

- ...:

  Additional parameters passed by `list_path_fileext()` to
  [`list.files()`](https://rdrr.io/r/base/list.files.html).
