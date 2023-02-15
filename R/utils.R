# @staticimports pkg:stringstatic
# str_extract str_replace str_replace_all str_width str_length str_pad
# str_remove str_detect

# @staticimports pkg:isstatic
# has_fileext has_all_names as_orientation as_cardinal_bearing str_add_fileext
# str_remove_fileext str_extract_fileext str_pad_digits str_increment_digits

.onLoad <- function(libname, pkgname) {
  utils::data(
    list = c(
      "default_tags"
    ),
    package = pkgname,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c(
    "img_width", "img_height"
  )
)
