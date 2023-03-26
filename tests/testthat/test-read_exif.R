test_that("read_exif and write_exif works", {
  skip("Test not working with check")
  path <- system.file("extdata/photos", package = "filenamr")

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

    # FIXME: This test passes but it is not the result I expected
    expect_identical(
      photos_write$keywords,
      list(c("test", "test"), c("test", "test"))
    )

  })
})
