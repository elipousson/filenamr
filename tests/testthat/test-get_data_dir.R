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

    expect_true(dir.exists("create-directory"))
  })

  skip_on_ci()
  expect_true(
    grepl("sfext", get_data_dir(cache = TRUE, pkg = "sfext"))
  )
})

test_that("get_data_dir respects quiet", {
  withr::with_tempdir({
    expect_no_message(
      get_data_dir(path = "quiet-directory", create = TRUE, quiet = TRUE)
    )
  })

  expect_warning(
    get_data_dir(path = "xyz", create = FALSE, quiet = TRUE)
  )
})

test_that("list_path_fileext works", {
  skip("Test not working with check")
  expect_identical(
    unique(list_path_fileext(system.file("R", package = "filenamr"))),
    "R"
  )
})

test_that("list_path_filenames lists files at a directory path", {
  withr::with_tempdir({
    writeLines("a", "a.txt")
    writeLines("b", "b.txt")

    result <- list_path_filenames(".", full.names = FALSE)

    expect_setequal(result, c("a.txt", "b.txt"))
  })
})

test_that("list_path_filenames filters by fileext", {
  withr::with_tempdir({
    writeLines("a", "a.txt")
    writeLines("b", "b.csv")

    result <- list_path_filenames(".", fileext = "txt", full.names = FALSE)

    expect_identical(result, "a.txt")
  })
})

test_that("list_path_filenames returns a single file path as-is", {
  withr::with_tempdir({
    writeLines("a", "a.txt")

    expect_identical(list_path_filenames("a.txt"), "a.txt")
  })
})

test_that("list_path_filenames accepts a data.frame with a path column", {
  withr::with_tempdir({
    writeLines("a", "a.txt")

    data <- data.frame(path = "a.txt")

    expect_identical(list_path_filenames(data), "a.txt")
  })
})

test_that("list_path_filenames errors for a nonexistent path", {
  expect_error(list_path_filenames("path/does/not/exist"))
})
