
<!-- README.md is generated from README.Rmd. Please edit that file -->

# filenamr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/elipousson/filenamr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/elipousson/filenamr?branch=main)
<!-- badges: end -->

The goal of filenamr is to help in creating and modifying file names and
paths. It also supports reading and writing EXIF metadata. It is
primarily useful for the development of packages that work with file
reading and writing and need some utilities to support this goal.

## Installation

You can install the development version of filenamr like so:

``` r
pak::pkg_install("elipousson/filenamr")
```

## Making file names

The `make_filename()` function is design for creating structured file
names based on a name and optional label, prefix, or postfix:

``` r
library(filenamr)

make_filename(
  name = "Neighborhoods",
  label = "Baltimore, MD"
)
#> [1] "baltimore_md_neighborhoods"

make_filename(
  prefix = "Plot",
  name = "Neighborhoods",
  label = "Baltimore, MD"
)
#> [1] "plot_baltimore_md_neighborhoods"

make_filename(
  prefix = "date",
  name = "Neighborhoods",
  label = "Baltimore, MD",
  postfix = "map",
  fileext = "jpeg"
)
#> [1] "2023-03-24_baltimore_md_neighborhoods_map.jpeg"
```

You can also pass a filename with an extension or path (setting
`create = TRUE` to create a directory if no directory already exist):

``` r
make_filename(
  filename = "baltimore_neighborhoods",
  path = "data",
  fileext = "pdf",
  create = FALSE
)
#> [1] "data/baltimore_neighborhoods.pdf"
```

If you provide a number as part of a filename, you can use the increment
parameter to create a filename where the number is replaced with a
larger number:

``` r
make_filename(
  filename = "plot",
  postfix = "1",
  increment = TRUE,
  fileext = "png"
)
#> [1] "plot_2.png"

make_filename(
  filename = "plot",
  postfix = "1",
  increment = 2,
  fileext = "png"
)
#> [1] "plot_3.png"
```

## Working with file metdata

This package uses the exiftoolr package (installing the development
version on GitHub is recommended) to support reading and writing EXIF,
IPTC, and XMP-DC metadata.
