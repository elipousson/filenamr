# Set a file path and validate path file extension

This is a flexible wrapper for `isstatic::file_path()` that allows you
to provide a file path as a filename or path parameter. If the path
contains a file extension and the fileext parameter is provided, the
function aborts if the two file extensions do not match.

## Usage

``` r
set_file_path(
  filename = NULL,
  path = NULL,
  fileext = NULL,
  allow_null = FALSE,
  call = caller_env()
)
```

## Arguments

- filename:

  File name. Optional if path is supplied.

- path:

  File path. Optional if filename is supplied.

- fileext:

  File extension. If the path supplied using filename and path *does
  not* end with a file extension, fileext is used as the file extension
  for the returned path. If the path, *does* end with a file extension,
  and fileext is used to validate the supplied.

- allow_null:

  If `TRUE`, return `NULL` if filename and path are `NULL`. If `FALSE`,
  error if filename and path are both `NULL`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.
