# Check if a file exists and remove file or error

The filename or path must include a single file extension.

## Usage

``` r
check_file_overwrite(
  filename = NULL,
  path = NULL,
  overwrite = TRUE,
  quiet = FALSE,
  ask = TRUE,
  .envir = caller_env(),
  call = caller_env()
)
```

## Arguments

- filename:

  File name, Default: `NULL`. Optional if path is supplied.

- path:

  File path, Default: `NULL`. Optional if filename is supplied.

- overwrite:

  If `TRUE`, remove a file with the same name and path

- quiet:

  If `TRUE`, suppress informational messages, Default: `FALSE`

- ask:

  If `TRUE`, overwrite is `FALSE`, and session is interactive, ask if
  user wants to overwrite the file. Default: `TRUE`

- .envir:

  Ignored at present.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.
