# @staticimports pkg:stringstatic
# str_replace_all

# @staticimports pkg:isstatic
# has_fileext file_path has_all_names as_cardinal_bearing str_add_fileext
# str_remove_fileext str_extract_fileext str_pad_digits str_increment_digits
# is_any str_n_freq is_file

.onLoad <- function(libname, pkgname) {
  utils::data(
    list = c(
      "default_exif_tags", "default_exif_xwalk"
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
