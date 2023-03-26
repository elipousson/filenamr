#' Format a data.frame column as a cardinal direction in degrees and wind direction
#'
#' @param data A data.frame or character vector to format with
#'   [isstatic::as_cardinal_bearing()]. If data is a data.frame, the photo
#'   direction is assumed to be in a column named "img_direction" and the new
#'   cardinal bearing is added to a column named "img_cardinal_wind".
#' @param winds Number of winds to use for results (4, 8, or 16).
#' @param .after Column name passed to .after parameter of [dplyr::mutate()].
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom rlang has_name
fmt_exif_direction <- function(data, winds = 8, .after = "img_direction", call = caller_env()) {
  if (is.character(data)) {
    return(as_cardinal_bearing(data, winds))
  }

  cli_abort_ifnot(
    "{.arg data} must be a {.cls character} or {.cls data.frame} object.",
    condition = is.data.frame(data),
    call = call
  )

  if (!rlang::has_name(data, c("img_direction"))) {
    return(data)
  }

  rlang::check_installed("dplyr")

  img_cardinal_dir <- as_cardinal_bearing(data$img_direction, winds)

  data$img_cardinal_dir <- img_cardinal_dir
  data$img_cardinal_wind <- names(img_cardinal_dir)

  dplyr::relocate(
    data,
    dplyr::all_of(c("img_cardinal_dir", "img_cardinal_wind")),
    .after = dplyr::all_of(.after)
  )
}

#' @noRd
fmt_exif_orientation <- function(data) {
  if (!has_all_names(
    data,
    c("exif_orientation", "img_width", "img_width")
  )) {
    return(data)
  }

  dplyr::mutate(
    data,
    exif_orientation =
      dplyr::case_when(
        exif_orientation == 1 ~ "Horizontal (normal)",
        exif_orientation == 2 ~ "Mirror horizontal",
        exif_orientation == 3 ~ "Rotate 180",
        exif_orientation == 4 ~ "Mirror vertical",
        exif_orientation == 5 ~ "Mirror horizontal and rotate 270 CW",
        exif_orientation == 6 ~ "Rotate 90 CW",
        exif_orientation == 7 ~ "Mirror horizontal and rotate 90 CW",
        exif_orientation == 8 ~ "Rotate 270 CW",
        TRUE ~ NA_character_
      ),
    # FIXME: as_orientation stopped working around 2023-03-20 - check and fix
    # orientation = as_orientation(img_width / img_height, 0),
    orientation = dplyr::case_when(
      (img_width / img_height) > 1 ~ "landscape",
      (img_width / img_height) < 1 ~ "portrait",
      (img_width / img_height) == 1 ~ "square",
      TRUE ~ NA_character_
    ),
    .after = "exif_orientation"
  )
}
