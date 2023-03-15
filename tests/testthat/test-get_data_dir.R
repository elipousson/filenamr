test_that("get_data_dir works", {

  expect_null(
    get_data_dir(path = NULL)
  )

  expect_error(
    get_data_dir(path = NULL, null.ok = FALSE)
  )

  withr::with_tempdir({
    expect_identical(
      get_data_dir(path = getwd()),
      getwd()
    )

    expect_identical(
      get_data_dir(path = "create-directory", create = TRUE),
      "create-directory"
    )

    dir.exists("create-directory")
  })
})
