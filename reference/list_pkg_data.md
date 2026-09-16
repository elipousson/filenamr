# List package datasets, extdata files, and files in package cache

`list_pkg_data()` calls three helper functions and returns a data.frame.
`list_pkg_datasets()` lists package datasets (using the library path as
the path), `list_pkg_extdata()` lists extdata files for a package (using
basename without a file extension as the item name),
`list_pkg_cachedata()` lists data in the package cache folder returned
by
[`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html).

## Usage

``` r
list_pkg_data(
  pkg = NULL,
  version = NULL,
  dir = "extdata",
  lib.loc = NULL,
  call = caller_env()
)

list_pkg_datasets(pkg, lib.loc = NULL)

list_pkg_extdata(pkg, dir = "extdata", full.names = TRUE, recursive = TRUE)

list_pkg_cachedata(pkg, full.names = TRUE, recursive = TRUE)
```

## Arguments

- pkg:

  The package names. Can include version requirements, e.g.
  `"pkg (>= 1.0.0)"`.

- version:

  Minimum versions for `pkg`. If supplied, must be the same length as
  `pkg`. `NA` elements stand for any versions.

- dir:

  One or more directory names to pass as the first argument of
  [`system.file()`](https://rdrr.io/r/base/system.file.html). Defaults
  to "extdir".

- lib.loc:

  a character vector with path names of R libraries. See ‘Details’ for
  the meaning of the default value of `NULL`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- full.names:

  a logical value. If `TRUE`, the directory path is prepended to the
  file names to give a relative file path. If `FALSE`, the file names
  (rather than paths) are returned.

- recursive:

  logical. Should the listing recurse into directories?
