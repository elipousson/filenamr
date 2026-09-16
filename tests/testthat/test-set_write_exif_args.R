test_that("set_write_exif_args builds title tags", {
  expect_identical(
    set_write_exif_args(title = "My Title"),
    c(
      "-Title=My Title",
      "-IPTC:Headline=My Title",
      "-IPTC:ObjectName=My Title",
      "-XMP-dc:Title=My Title",
      "-overwrite_original"
    )
  )
})

test_that("set_write_exif_args builds author and credit tags", {
  expect_identical(
    set_write_exif_args(author = "Jane"),
    c(
      "-Author=Jane",
      "-XMP-dc:creator=Jane",
      "-IPTC:Credit=Jane",
      "-XMP-dc:Credit=Jane",
      "-overwrite_original"
    )
  )
})

test_that("set_write_exif_args toggles keyword overwrite vs append operators", {
  expect_identical(
    set_write_exif_args(keywords = "a,b", overwrite = TRUE, append_keywords = FALSE),
    c("-IPTC:Keywords=a,b", "-XMP-dc:Subject=a,b", "-overwrite_original")
  )

  expect_identical(
    set_write_exif_args(keywords = "a,b", overwrite = FALSE, append_keywords = TRUE),
    c("-IPTC:Keywords+=a,b", "-XMP-dc:Subject+=a,b")
  )
})

test_that("set_write_exif_args adds a PNG-specific alt text tag", {
  args <- set_write_exif_args(alt = "alt text", fileext = "png")

  expect_true("-iTXt=alt text" %in% args)
})

test_that("set_write_exif_args omits the PNG-specific alt tag for other file types", {
  args <- set_write_exif_args(alt = "alt text", fileext = "jpeg")

  expect_false(any(grepl("^-iTXt=", args)))
})

test_that("set_write_exif_args reads values from metadata when direct args are NULL", {
  expect_identical(
    set_write_exif_args(metadata = list(title = "T", author = "A")),
    c(
      "-Title=T",
      "-IPTC:Headline=T",
      "-IPTC:ObjectName=T",
      "-XMP-dc:Title=T",
      "-Author=A",
      "-XMP-dc:creator=A",
      "-IPTC:Credit=A",
      "-XMP-dc:Credit=A",
      "-overwrite_original"
    )
  )
})

test_that("set_write_exif_args prefers direct arguments over metadata", {
  args <- set_write_exif_args(title = "Direct", metadata = list(title = "FromMetadata"))

  expect_true("-Title=Direct" %in% args)
  expect_false(any(grepl("FromMetadata", args)))
})

test_that("set_write_exif_args omits overwrite_original flag when overwrite is FALSE", {
  args <- set_write_exif_args(title = "T", overwrite = FALSE)

  expect_false("-overwrite_original" %in% args)
})

test_that("set_write_exif_args errors when no tag values or args are supplied", {
  expect_error(
    set_write_exif_args(),
    "must be supplied"
  )
})
