test_that("set_file_path works", {
  expect_error(
    set_file_path()
  )

  expect_message(
    set_file_path("filename.jpeg", "path.png")
  )

  expect_identical(
    set_file_path("filename", fileext = "jpeg"),
    "filename.jpeg"
  )

  expect_error(
    set_file_path("filename.jpeg", fileext = "png")
  )
})

test_that("set_file_path returns path unchanged when fileext already matches", {
  expect_identical(
    set_file_path("filename.jpeg", fileext = "jpeg"),
    "filename.jpeg"
  )
})

test_that("set_file_path returns NULL when filename and path are NULL and allow_null is TRUE", {
  expect_null(set_file_path(allow_null = TRUE))
})

test_that("set_file_path combines filename and path", {
  expect_identical(
    set_file_path("filename.csv", "some/dir"),
    file.path("some/dir", "filename.csv")
  )
})
