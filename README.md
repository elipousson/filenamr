
<!-- README.md is generated from README.Rmd. Please edit that file -->

# filenamr

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MT](https://img.shields.io/badge/license-MT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
<!-- badges: end -->

The goal of filenamr is to …

## Installation

You can install the development version of filenamr like so:

``` r
pak::pkg_install("elipousson/sfext")
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
#> [1] "2022_11-25_baltimore_md_neighborhoods_map.jpeg"
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
