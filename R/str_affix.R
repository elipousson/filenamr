#' Apply a prefix or postfix to a string
#'
#' Prefix and postfix can include more than one value that are added in the same
#' order provided. For [str_affix()], the string must be a single character
#' string.
#'
#' - [str_affix()]: Add a label, prefix, and postfix to string
#' - [str_prefix()]: Add a prefix or a postfix to a string
#'
#' @param prefix Character string or character vector to add to string parameter
#'   as a prefix.
#' @param string A single string that the attach prefix or postfix is added to.
#' @param postfix Character string or character vector to add to string
#'   parameter as a postfix.
#' @param sep Separator character passed as the collapse parameter of [paste()].
#' @inheritParams str_pad_digits
#' @param use_clean_names If `TRUE`, prefix, postfix, and string are all
#'   converted to snake case with [janitor::make_clean_names()].
#' @inheritParams janitor::make_clean_names
#' @name str_affix
#' @rdname str_affix
#' @export
str_affix <- function(string = NULL,
                      prefix = NULL,
                      postfix = NULL,
                      sep = "_",
                      pad = NULL,
                      width = NULL,
                      use_clean_names = TRUE,
                      case = "snake",
                      replace = c(`'` = "", `"` = "", `%` = "_pct_", `#` = "_num_"),
                      use_make_names = TRUE,
                      ...) {
  cli_abort_ifnot(
    "{.arg prefix} must be a {.cls character} object or NULL." = is.character(prefix) || is.null(prefix),
    "{.arg string} must be a {.cls character} object or NULL." = is.character(string) || is.null(string),
    "{.arg postfix}  must be a {.cls character} object or NULL." = is.character(postfix) || is.null(postfix)
  )

  string <- str_pad_digits(string, pad = pad, width = width)

  if (use_clean_names) {
    rlang::check_installed("janitor")
    # FIXME: make_clean_names has an optional prefix and postfix parameter - can
    # I use those instead of the custom str_fix functions
    string <- janitor::make_clean_names(string, use_make_names = use_make_names)
  }

  # Add prefix and postfix
  string <-
    str_prefix(string,
      prefix,
      sep = sep,
      use_clean_names = use_clean_names,
      use_make_names = use_make_names,
      ...
    )
  string <-
    str_prefix(string,
      postfix,
      is_postfix = TRUE,
      sep = sep,
      use_clean_names = use_clean_names,
      use_make_names = use_make_names,
      ...
    )

  # Remove double separators
  gsub(paste0(sep, "{2}"), sep, string)
}

#' @name str_prefix
#' @rdname str_affix
#' @param is_postfix If `TRUE`, use the prefix string as a postfix; defaults to
#'   `FALSE`.
#' @param date.format,time.format Date or time format. Only used by [str_prefix]
#'   if prefix is "date" or "time" and not currently accessible when using
#'   [str_affix()] or [make_filename()].
#' @param ... Additional parameters passed to janitor::make_clean_names() if
#'   use_clean_names is `TRUE`.
#' @export
str_prefix <- function(string = NULL,
                       prefix = NULL,
                       sep = "_",
                       is_postfix = FALSE,
                       date.format = "%F",
                       time.format = "%Y-%m-%d_%I-%M-%S_%p",
                       use_clean_names = TRUE,
                       case = "snake",
                       replace = c(`'` = "", `"` = "", `%` = "_pct_", `#` = "_num_"),
                       use_make_names = TRUE,
                       ...) {
  if (is.null(prefix)) {
    return(string)
  }

  if (prefix %in% c("date", "time") && !is.null(c(date.format, time.format))) {
    prefix <-
      switch(prefix,
        "date" = Sys.Date(),
        "time" = Sys.time()
      )
  }

  if (inherits(prefix, "Date")) {
    prefix <- format(prefix, date.format)
  }

  if (inherits(prefix, "POSIXct")) {
    prefix <- format(prefix, time.format)
  }

  if (use_clean_names) {
    rlang::check_installed("janitor")

    prefix <-
      janitor::make_clean_names(
        prefix, case, replace,
        use_make_names = use_make_names,
        ...
      )
  }

  if (is_postfix) {
    return(paste(c(string, prefix), collapse = sep))
  }

  paste(c(prefix, string), collapse = sep)
}
