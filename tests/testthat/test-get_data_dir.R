test_that("get_data_dir works", {
  expect_null(
    get_data_dir(path = NULL)
  )

  expect_error(
    get_data_dir(path = NULL, allow_null = FALSE)
  )

  expect_warning(
    get_data_dir(
      path = "xyz",
      create = FALSE
    )
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

  skip_on_ci()
  expect_true(
    grepl("sfext", get_data_dir(cache = TRUE, pkg = "sfext"))
  )
})


test_that("list_path_fileext works", {
  skip("Test not working with check")
  expect_identical(
    unique(list_path_fileext(system.file("R", package = "filenamr"))),
    "R"
  )
})
