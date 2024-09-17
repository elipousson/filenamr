test_that("read_exif and write_exif works", {
  skip_on_ci()

  exiftool_v <- exiftoolr::exif_version()

  skip_if_not(is.character(exiftool_v))

  path <- system.file("images", package = "exiftoolr")

  photos <- read_exif(path)

  expect_s3_class(
    photos,
    "data.frame"
  )

  withr::with_tempdir({
    tempfiles <- basename(list.files(path))

    file.copy(
      photos$path,
      tempfiles
    )

    write_exif(
      path = tempfiles,
      fileext = "jpeg",
      keywords = "test",
      overwrite = TRUE
    )

    photos_write <- read_exif(tempfiles)

    expect_identical(
      photos_write$keywords,
      c("test", "test", "test")
    )
  })
})
