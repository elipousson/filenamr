#' Modify digits within strings
#'
#' @description
#' - [str_replace_digits()]: Replace digits with a string optionally
#' incrementing the digits
#' - [str_pad_digits()]: Pad a string with digits
#' - [str_extract_digits()]: Extract digits from a string
#'
#' @name str_digits
NULL

#' @param pad Single padding character added to digits in string; defaults to
#'   "0"
#' @inheritParams stringstatic::str_pad
#' @name str_pad_digits
#' @rdname str_digits
#' @export
str_pad_digits <- function(string, pad = "0", side = "left", width = NULL) {
  if (is.null(pad)) {
    return(string)
  }

  digits <- str_extract_digits(string)

  width <- width %||% max(nchar(digits))

  digits <- str_pad(digits, width, side, pad)

  str_replace(string, digit_pattern(), digits)
}

#' @name str_extract_digits
#' @rdname str_digits
#' @export
str_extract_digits <- function(string, pattern = "[0-9]+", side = NULL) {
  str_extract(string, digit_pattern(pattern, side))
}

#' @name str_replace_digits
#' @rdname str_digits
#' @inheritParams stringstatic::str_replace
#' @export
str_replace_digits <- function(string,
                               replacement,
                               pad = "0",
                               side = "left",
                               width = NULL) {
  digits <- str_extract_digits(string)

  if (is.na(digits)) {
    return(string)
  }

  if (!is.null(width)) {
    replacement <- str_pad_digits(replacement, pad, side, width)
  }

  str_replace(string, digits, replacement)
}

#' Helper to return a regex based on side
#'
#' @noRd
digit_pattern <- function(pattern = "[0-9]+", side = NULL) {
  side <- match.arg(side, c("", "left", "right"))
  switch(side,
    "left" = paste0("^", pattern),
    "right" = paste0(pattern, "$"),
    pattern
  )
}

#' @name str_increment_digits
#' @rdname str_digits
#' @param increment If `TRUE`, increment digits in string by 1. If numeric,
#'   increment digits in string by value. If `NULL`, 0, or if no digits are
#'   present in string, return string as is.
#' @param ... Passed to [str_replace_digits()]
#' @export
str_increment_digits <- function(string, increment = TRUE, ...) {
  if (is.logical(increment)) {
    if (increment) {
      increment <- 1
    } else {
      increment <- NULL
    }
  }

  digits <- str_extract_digits(string)

  if (any(c(is.na(digits), is.null(increment), increment == 0))) {
    return(string)
  }

  replacement <- as.numeric(digits) + increment

  str_replace_digits(string, replacement = as.character(replacement), ...)
}
