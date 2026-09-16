# Apply a prefix or postfix to a string

Prefix and postfix can include more than one value that are added in the
same order provided. For `str_affix()`, the string must be a single
character string.

## Usage

``` r
str_affix(
  string = NULL,
  prefix = NULL,
  postfix = NULL,
  sep = "_",
  pad = NULL,
  width = NULL,
  use_clean_names = TRUE,
  case = "snake",
  replace = c(`'` = "", `"` = "", `%` = "_pct_", `#` = "_num_"),
  use_make_names = TRUE,
  call = caller_env(),
  ...
)

str_prefix(
  string = NULL,
  prefix = NULL,
  sep = "_",
  is_postfix = FALSE,
  date.format = "%Y-%m-%d",
  time.format = "%Y-%m-%d_%I-%M-%S_%p",
  use_clean_names = TRUE,
  case = "snake",
  replace = c(`'` = "", `"` = "", `%` = "_pct_", `#` = "_num_"),
  use_make_names = TRUE,
  ...
)
```

## Arguments

- string:

  A single string that the attach prefix or postfix is added to.

- prefix:

  Character string or character vector to add to string parameter as a
  prefix.

- postfix:

  Character string or character vector to add to string parameter as a
  postfix.

- sep:

  Separator character passed as the collapse parameter of
  [`paste()`](https://rdrr.io/r/base/paste.html).

- pad:

  Single padding character added to digits in string; defaults to "0"

- width:

  Minimum width of padded strings.

- use_clean_names:

  If `TRUE`, prefix, postfix, and string are all converted to snake case
  with
  [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html).

- case:

  The desired target case (default is `"snake"`) will be passed to
  [`snakecase::to_any_case()`](https://rdrr.io/pkg/snakecase/man/to_any_case.html)
  with the exception of "old_janitor", which exists only to support
  legacy code (it preserves the behavior of `clean_names()` prior to
  addition of the "case" argument (janitor versions \<= 0.3.1).
  "old_janitor" is not intended for new code. See
  [`to_any_case`](https://rdrr.io/pkg/snakecase/man/to_any_case.html)
  for a wide variety of supported cases, including "sentence" and
  "title" case.

- replace:

  A named character vector where the name is replaced by the value.

- use_make_names:

  Should [`make.names()`](https://rdrr.io/r/base/make.names.html) be
  applied to ensure that the output is usable as a name without quoting?
  (Avoiding [`make.names()`](https://rdrr.io/r/base/make.names.html)
  ensures that the output is locale-independent but quoting may be
  required.)

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- ...:

  Additional parameters passed to janitor::make_clean_names() if
  use_clean_names is `TRUE`.

- is_postfix:

  If `TRUE`, use the prefix string as a postfix; defaults to `FALSE`.

- date.format, time.format:

  Date or time format. Only used by str_prefix if prefix is "date" or
  "time" and not currently accessible when using `str_affix()` or
  [`make_filename()`](https://elipousson.github.io/filenamr/reference/make_filename.md).

## Details

- `str_affix()`: Add a label, prefix, and postfix to string

- `str_prefix()`: Add a prefix or a postfix to a string
