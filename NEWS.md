# filenamr (development)

- Fix issue with `check_file_overwrite()` caused by missing .envir parameter (2023-09-18)

# filenamr 0.1.0.9002 (2023-04-06)

## Features

- Add creator and alt parameters to `read_exif()`. Update handling of args so the other values are not ignored if they are not `NULL`.
- Export `list_path_filenames()` and `list_path_fileext()`.
- Add `list_pkg_data()` function.

## Refactoring

- Move `rappdirs` from Suggests to Imports.
- Drop `sf` package from Suggests (remove geometry parameter from `read_exif()`)
- Pass call parameter for more consistent error-handling for all functions.

## Testing

- Rework `read_exif()` test to use `exiftoolr` sample images.

# filenamr 0.1.0.9001 (2023-03-20)

* Export `check_path_fileext()` function.
* Remove `str_fileext` functions (moved to [{isstatic}](https://github.com/elipousson/isstatic) package)

# filenamr 0.1.0.9000 (2023-03-15)

* Initial release.

