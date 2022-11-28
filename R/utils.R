# @staticimports pkg:stringstatic
# str_extract str_replace str_width str_length str_pad str_remove str_detect

# @staticimports pkg:isstatic
# has_fileext

utils::globalVariables(
  c(
    "img_direction", "img_cardinal_dir"
  )
)

#' Is this package installed?
#'
#' @param pkg Name of a package.
#' @param repo GitHub repository to use for the package.
#' @noRd
#' @importFrom rlang check_installed
is_pkg_installed <- function(pkg, repo = NULL) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(TRUE))
  }
  if (!is.null(repo)) {
    pkg <- repo
  }

  check_installed(pkg = pkg)
}
