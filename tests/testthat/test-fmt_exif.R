test_that("fmt_exif_direction works with a character vector", {
  result <- fmt_exif_direction(c("0", "90", "180", "270"))

  expect_identical(unname(result), c(0, 90, 180, 270))
  expect_identical(names(result), c("N", "E", "S", "W"))
})

test_that("fmt_exif_direction respects the winds argument", {
  result <- fmt_exif_direction(c("0", "90", "180", "270"), winds = 4)

  expect_identical(names(result), c("N", "E", "S", "W"))
})

test_that("fmt_exif_direction errors for unsupported input types", {
  expect_error(
    fmt_exif_direction(list(img_direction = 90)),
    class = "rlang_error"
  )
})

test_that("fmt_exif_direction returns data.frame unchanged if img_direction is missing", {
  data <- data.frame(x = 1:3)

  expect_identical(fmt_exif_direction(data), data)
})

test_that("fmt_exif_direction adds cardinal direction columns to a data.frame", {
  skip_if_not_installed("dplyr")

  data <- data.frame(img_direction = c(0, 90, 180, 270), x = 1:4)

  result <- fmt_exif_direction(data)

  expect_true(all(c("img_cardinal_dir", "img_cardinal_wind") %in% names(result)))
  expect_identical(result$img_cardinal_dir, c(0, 90, 180, 270))
  expect_identical(result$img_cardinal_wind, c("N", "E", "S", "W"))
})

test_that("fmt_exif_orientation decodes orientation and aspect ratio", {
  skip_if_not_installed("dplyr")

  data <- data.frame(
    exif_orientation = c(1, 6, 3),
    img_width = c(100, 50, 20),
    img_height = c(50, 100, 20)
  )

  result <- fmt_exif_orientation(data)

  expect_identical(
    result$exif_orientation,
    c("Horizontal (normal)", "Rotate 90 CW", "Rotate 180")
  )
  expect_identical(result$orientation, c("landscape", "portrait", "square"))
})

test_that("fmt_exif_orientation returns data.frame unchanged if columns are missing", {
  data <- data.frame(x = 1)

  expect_identical(fmt_exif_orientation(data), data)
})
