# Generated by staticimports; do not edit by hand.
# ======================================================================
# Imported from pkg:isstatic
# ======================================================================

#' Convert a numeric bearing value to the closest cardinal bearing
#'
#' @param x A numeric vector with degrees or a data.frame with column name
#'   matching the first name in cols.
#' @param winds Number of winds to use for results (4, 8, or 16).
#' @param cols A length 2 character vector where the first value is a column
#'   name containing bearing values and the second is the name of the new column
#'   added to the data.frame. Required if x is a data.frame.
#' @returns A named numeric vector with cardinal bearings (and wind names) or a
#'   data.frame with an added column containing the cardinal bearings.
#' @noRd
as_cardinal_bearing <- function(x,
                                winds = 8,
                                cols = c("bearing", "cardinal_bearing")) {
  if (is.data.frame(x)) {
    check_name(x, cols[1])
    x[[cols[2]]] <- as_cardinal_bearing(x[[cols[1]]], winds)
    return(x)
  }

  check_numeric(x)
  check_if(
    condition = winds %in% c(4, 8, 16),
    "`winds` must be 4, 8, or 16."
  )

  wind_degrees <- sort(cardinal_bearings[c(1:(winds + 1))])

  sapply(
    x,
    function(i) {
      wind_degrees[findInterval(i, wind_degrees - (360 / (winds * 2)))]
    }
  )
}

#' What is the orientation of a numeric aspect ratio?
#'
#' @param x A numeric vector with an aspect ratio or a data.frame with width and
#'   height column (using width and height values from columns matching the cols
#'   parameter).
#' @param tolerance Positive numeric value above or below 1 used to determine if
#'   an aspect ratio is square, landscape, or portrait.
#' @param cols Name of width and height column if x is a data.frame object.
#' @returns A character vector of orientations of the same length as x or, if x
#'   is a data.frame, the same length as the number of rows in x.
#' @noRd
as_orientation <- function(x, tolerance = 0.1, cols = c("width", "height")) {
  tolerance <- abs(tolerance)

  if (is.data.frame(x)) {
    check_name(x, cols)
    return(
      as_orientation(
        as.numeric(x[, cols[1]]) / as.numeric(x[, cols[2]]),
        tolerance
      )
    )
  }

  check_numeric(x)

  if (length(x) > 1) {
    return(map_chr(x, as_orientation, tolerance))
  }

  if (x > (1 + tolerance)) {
    return("landscape")
  }

  if (x < (1 - tolerance)) {
    return("portrait")
  }

  "square"
}

cardinal_bearings <-
  c(
    "N" = 0, "N" = 360, "E" = 90,
    "S" = 180, "W" = 270,
    "NE" = 45, "SE" = 135,
    "SW" = 225, "NW" = 315,
    "NNE" = 22.5, "ENE" = 67.5, "ESE" = 112.5,
    "SSE" = 157.5, "SSW" = 202.5, "WSW" = 247.5,
    "WNW" = 292.5, "NNW" = 337.5
  )

#' @noRd
check_if <- function(condition, message = NULL, call = parent.frame()) {
  if (isTRUE(condition)) {
    return(invisible(NULL))
  }

  stop(
    message,
    call. = call
  )
}

#' @noRd
check_name <- function(x, name = NULL, call = parent.frame()) {
  check_if(
    condition = has_all_names(x, name),
    message = paste0(
      "`x` must have ", plural_words("name", length(name), after = " "), name,
      ", but ", combine_words(name[!(name %in% names(x))]), " are all missing."
    ),
    call = call
  )
}

#' @noRd
check_numeric <- function(x, call = parent.frame()) {
  check_if(
    condition = all(is.numeric(x[!is.na(x)])),
    message = paste("`x` must be a <numeric> vector, not", class(x)),
    call = call
  )
}

#' Combine multiple words into a single string
#'
#' @author Yihui Xie \email{xie@yihui.name}
#'   ([ORCID](https://orcid.org/0000-0003-0645-5666))
#'
#' @source Adapted from [knitr::combine_words()] in the
#'   [knitr](https://yihui.org/knitr/) package.
#'
#' @inherit knitr::combine_words
#' @returns A character string
#' @noRd
combine_words <- function(words,
                          sep = ", ",
                          and = " and ",
                          before = "",
                          after = before,
                          oxford_comma = TRUE) {
  n <- length(words)

  rs <- function (x) {
    if (is.null(x))
      x = as.character(x)
    x
  }

  if (n == 0) {
    return(words)
  }

  words <- paste0(before, words, after)

  if (n == 1) {
    return(rs(words))
  }

  if (n == 2) {
    return(rs(paste(words, collapse = if (is_blank(and)) sep else and)))
  }

  if (oxford_comma && grepl("^ ", and) && grepl(" $", sep)) {
    and <- gsub("^ ", "", and)
  }

  words[n] <- paste0(and, words[n])

  if (!oxford_comma) {
    words[n - 1] <- paste0(words[n - 1:0], collapse = "")
    words <- words[-n]
  }

  rs(paste(words, collapse = sep))
}

#' Does an object have all of the provided names?
#'
#' @param x A data frame or another named object.
#' @param name Element name(s) to check.
#' @noRd
has_all_names <- function(x, name) {
  if (anyNA(c(x, name))) {
    return(FALSE)
  }

  all(utils::hasName(x, name))
}

#' Does string contain the specified file type or any file extension?
#'
#' Check if string contains any filetype or the provided filetype. If string is
#' `NULL`, returns `FALSE`.
#'
#' @param string String to be tested with or without filetype. Defaults to
#'   `NULL`.
#' @param fileext File type to test against. Optional.
#' @param ignore.case If `FALSE`, the pattern matching is case sensitive. If
#'   `TRUE`, case is ignored.
#' @noRd
has_fileext <- function(string = NULL, fileext = NULL, ignore.case = FALSE) {
  if (is.null(string)) {
    return(FALSE)
  }

  if (is.null(fileext)) {
    fileext <- "[a-zA-Z0-9]+"
  }

  is_fileext_path(string, fileext, ignore.case)
}

#' @inherit xfun::is_blank
#'
#' @author Yihui Xie \email{xie@yihui.name}
#'   ([ORCID](https://orcid.org/0000-0003-0645-5666))
#'
#' @source Adapted from [xfun::is_blank()] in the
#'   [xfun](https://yihui.org/xfun/) package.
#'
#' @examples
#' is_blank("")
#' is_blank("abc")
#' is_blank(c("", "  ", "\n\t"))
#' is_blank(c("", " ", "abc"))
#' @noRd
is_blank <- function(x) {
  all(grepl("^\\s*$", x))
}

#' Is this a file path or url ending in the specified file extension?
#'
#' @param x A character vector to check.
#' @param fileext A file extension (or multiple file extensions) to compare to
#'   x. Required.
#' @inheritParams base::grepl
#' @noRd
is_fileext_path <- function(x, fileext, ignore.case = TRUE) {
  grepl(
    paste0("\\.", paste0(fileext, collapse = "|"), "$(?!\\.)"),
    x,
    ignore.case = ignore.case, perl = TRUE
  )
}

#' Apply a function to each element of a vector.
#'
#' @author Winston Chang \email{winston@stdout.org}
#'
#' @source [purr-like functions](https://github.com/wch/staticimports/blob/main/inst/staticexports/purrr.R) in [staticimports](https://wch.github.io/staticimports/) package
#
#' @noRd
map_chr <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA_character_)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA_character_)
  }
}

#' Simple helper for pluralizing words
#'
#' @noRd
plural_words <- function(words,
                         n = 1,
                         suffix = "s",
                         before = "",
                         after = "",
                         replacement = NULL) {
  words <- paste0(before, words, after)

  if (is.null(replacement)) {
    replacement <- paste0(words, suffix)
  }

  if (n > 1) {
    return(replacement)
  }

  words
}
# Generated by staticimports; do not edit by hand.
# ======================================================================
# Imported from pkg:stringstatic
# ======================================================================

#' Detect the presence or absence of a pattern in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_detect()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @param negate If `TRUE`, return non-matching elements.
#'
#' @return A logical vector.
#' @noRd
str_detect <- function(string, pattern, negate = FALSE) {
	is_fixed <- inherits(pattern, "fixed")
	ignore.case <- isTRUE(attr(pattern, "options")$case_insensitive)

	if (length(string) == 0 || length(pattern) == 0) return(logical(0))

	indices <- Vectorize(grep, c("pattern", "x"), USE.NAMES = FALSE)(
		pattern,
		x = string,
		ignore.case = ignore.case,
		perl = !is_fixed,
		fixed = is_fixed,
		invert = negate
	)

	result <- as.logical(lengths(indices))
	result[is.na(string)] <- NA
	result
}

#' Extract matching patterns from a string
#'
#' Dependency-free drop-in alternative for `stringr::str_extract()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @return A character matrix.
#'   The first column is the complete match,
#'   followed by one column for each capture group.
#' @noRd
str_extract <- function(string, pattern) {
	ignore.case <- isTRUE(attr(pattern, "options")$case_insensitive)
	is_fixed <- !ignore.case && inherits(pattern, "fixed")

	if (length(string) == 0 || length(pattern) == 0) return(character(0))

	result <- Map(
		function(string, pattern) {
			if (is.na(string) || is.na(pattern)) return(NA_character_)

			regmatches(
				x = string,
				m = regexpr(
					pattern = pattern,
					text = string,
					ignore.case = ignore.case,
					perl = !is_fixed,
					fixed = is_fixed
				)
			)
		},
		string, pattern, USE.NAMES = FALSE
	)

	result[lengths(result) == 0] <- NA_character_
	unlist(result)
}

#' Compute the length of a string
#'
#' Dependency-free drop-in alternative for `stringr::str_length()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @return A numeric vector the same length as string.
#' @noRd
str_length <- function(string) {
	nchar(as.character(string), type = "chars", keepNA = TRUE)
}

#' Duplicate and concatenate strings within a character vector
#'
#' Dependency-free drop-in alternative for `stringr::str_pad()`.
#'
#' @author Eli Pousson \email{eli.pousson@gmail.com}
#'   ([ORCID](https://orcid.org/0000-0001-8280-1706))
#'
#'   Alexander Rossell Hayes \email{alexander@rossellhayes.com}
#'   ([ORCID](https://orcid.org/0000-0001-9412-0457))
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#' @param width Minimum width of padded strings.
#' @param side Side on which padding character is added (left, right or both).
#' @param pad Single padding character (default is a space).
#' @param use_width If `FALSE`,
#'   use the length of the string instead of the width;
#'   see [str_width()]/[str_length()] for the difference.
#'
#' @return A character vector.
#' @noRd
str_pad <- function(
		string, width, side = c("left", "right", "both"), pad = " ", use_width = TRUE
) {
	if (!is.numeric(width)) {
		return(string[NA])
	}

	if (any(nchar(pad, type = "width") != 1)) {
		stop("each string in `pad` should consist of code points of total width 1")
	}

	side <- match.arg(side)

	nchar_type <- if (isTRUE(use_width)) "width" else "chars"
	string_width <- nchar(string, nchar_type)
	pad_width <- width - string_width
	pad_width[pad_width < 0] <- 0

	switch(
		side,
		"left" = paste0(strrep(pad, pad_width), string),
		"right" = paste0(string, strrep(pad, pad_width)),
		"both" = paste0(
			strrep(pad, floor(pad_width / 2)),
			string,
			strrep(pad, ceiling(pad_width / 2))
		)
	)
}

#' Remove matched patterns in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_remove()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @return A character vector.
#' @noRd
str_remove <- function(string, pattern) {
	ignore.case <- isTRUE(attr(pattern, "options")$case_insensitive)
	is_fixed <- !ignore.case && inherits(pattern, "fixed")

	sub <- Vectorize(sub, c("pattern", "x"), USE.NAMES = FALSE)

	sub(
		pattern,
		replacement = "",
		x = string,
		ignore.case = ignore.case,
		perl = !is_fixed,
		fixed = is_fixed
	)
}

#' Replace matched patterns in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_replace()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @param replacement A character vector of replacements.
#'   Should be either length one, or the same length as `string` or `pattern`.
#'   References of the form `\1`, `\2`, etc. will be replaced with the contents
#'   of the respective matched group (created by `()`).
#'
#'   To replace the complete string with `NA`,
#'   use `replacement = NA_character_`.
#'
#'   Using a function for `replacement` is not yet supported.
#'
#' @return A character vector.
#' @noRd
str_replace <- function(string, pattern, replacement) {
	ignore.case <- isTRUE(attr(pattern, "options")$case_insensitive)
	is_fixed <- !ignore.case && inherits(pattern, "fixed")

	sub <- Vectorize(sub, c("pattern", "replacement", "x"), USE.NAMES = FALSE)

	sub(
		pattern,
		replacement,
		x = string,
		ignore.case = ignore.case,
		perl = !is_fixed,
		fixed = is_fixed
	)
}

#' Compute the width of a string
#'
#' Dependency-free drop-in alternative for `stringr::str_width()`.
#' Results for non-ASCII characters may be inaccurate in R < 4.0.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @return A numeric vector the same length as string.
#' @noRd
str_width <- function(string) {
	nchar(as.character(string), type = "width", keepNA = TRUE)
}
