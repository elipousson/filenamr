# Check if a file path has a file extension

`check_path_fileext()` checks if a character vector of file paths have a
file extension or a specified file extension. Errors if any elements of
do not pass the condition.

## Usage

``` r
check_path_fileext(
  path,
  fileext = NULL,
  message = "{.arg {arg}} must have a file extension.",
  arg = caller_arg(path),
  call = caller_env()
)
```

## Arguments

- path:

  Character vector with file path or paths to check. Required.

- fileext:

  Optional file extension string. If `NULL`, path must have a file
  extension. If fileext is a character string, all elements of path must
  have a matching file extension.

- message:

  It is formatted via a call to
  [`cli_bullets()`](https://cli.r-lib.org/reference/cli_bullets.html).

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  `call = caller_env()`. The corresponding function call is retrieved
  and mentioned in error messages as the source of the error.

  You only need to supply `call` when throwing a condition from a helper
  function which wouldn't be relevant to mention in the message.

  Can also be `NULL` or a [defused function
  call](https://rlang.r-lib.org/reference/topic-defuse.html) to
  respectively not display any call or hard-code a code to display.

  For more information about error calls, see [Including function calls
  in error
  messages](https://rlang.r-lib.org/reference/topic-error-call.html).
