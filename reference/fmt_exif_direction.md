# Format a data.frame column as a cardinal direction in degrees and wind direction

Format a data.frame column as a cardinal direction in degrees and wind
direction

## Usage

``` r
fmt_exif_direction(
  data,
  winds = 8,
  .after = "img_direction",
  call = caller_env()
)
```

## Arguments

- data:

  A data.frame or character vector to format with
  `isstatic::as_cardinal_bearing()`. If data is a data.frame, the photo
  direction is assumed to be in a column named "img_direction" and the
  new cardinal bearing is added to a column named "img_cardinal_wind".

- winds:

  Number of winds to use for results (4, 8, or 16).

- .after:

  Column name passed to .after parameter of
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## See also

`isstatic::as_cardinal_bearing()`
