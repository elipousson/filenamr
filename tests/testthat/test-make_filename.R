test_that("make_filename works", {
  expect_identical(make_filename(name = "data", fileext = "csv"), "data.csv")
  expect_identical(make_filename(filename = "data.csv"), "data.csv")
  expect_identical(
    make_filename(name = "Data", label = "Source", fileext = "geojson"),
    "source_data.geojson"
  )
  expect_identical(
    make_filename(name = "Data", postfix = "Source", fileext = "geojson"),
    "data_source.geojson"
  )
  expect_identical(
    make_filename(
      name = "Data file",
      prefix = as.Date("2023-02-09"),
      fileext = "csv"
    ),
    "2023-02-09_data_file.csv"
  )
  expect_identical(
    make_filename(
      name = "Data file",
      prefix = as.POSIXct("2023-02-09 09:00 AM"),
      fileext = "csv"
    ),
    "2023-02-09_09-00-00_am_data_file.csv"
  )
  expect_identical(
    make_filename(
      name = "data.csv"
    ),
    "data.csv"
  )
  expect_identical(
    make_filename(
      path = "data.csv"
    ),
    "data.csv"
  )
  expect_error(
    make_filename(
      name = "Data file",
      allow_null_fileext = FALSE
    )
  )
})

test_that("make_filename warns", {
  expect_warning(
    make_filename(name = "data", fileext = "csv", filename = "data.csv"),
    "`filename` is ignored if a `name` argument is provided."
  )
  expect_warning(make_filename(filename = "data.csv", label = "source"))
  expect_warning(make_filename(filename = "data.csv", path = "folder", create = FALSE))
})
