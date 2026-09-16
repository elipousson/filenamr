# Changelog

## filenamr 0.1.0.9002 (2023-04-06)

### Features

- Add creator and alt parameters to
  [`read_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.md).
  Update handling of args so the other values are not ignored if they
  are not `NULL`.
- Export
  [`list_path_filenames()`](https://elipousson.github.io/filenamr/reference/get_data_dir.md)
  and
  [`list_path_fileext()`](https://elipousson.github.io/filenamr/reference/get_path_fileext.md).
- Add
  [`list_pkg_data()`](https://elipousson.github.io/filenamr/reference/list_pkg_data.md)
  function.

### Refactoring

- Move `rappdirs` from Suggests to Imports.
- Drop `sf` package from Suggests (remove geometry parameter from
  [`read_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.md))
- Pass call parameter for more consistent error-handling for all
  functions.

### Testing

- Rework
  [`read_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.md)
  test to use `exiftoolr` sample images.

## filenamr 0.1.0.9001 (2023-03-20)

- Export
  [`check_path_fileext()`](https://elipousson.github.io/filenamr/reference/check_path_fileext.md)
  function.
- Remove `str_fileext` functions (moved to
  [{isstatic}](https://github.com/elipousson/isstatic) package)

## filenamr 0.1.0.9000 (2023-03-15)

- Initial release.
