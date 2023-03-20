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
