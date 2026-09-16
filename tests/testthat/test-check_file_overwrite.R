test_that("check_file_overwrite works", {
  withr::with_tempdir({
    writeLines("test text", "test-text.txt")
    expect_message(
      check_file_overwrite(
        "test-text.txt",
        ask = FALSE,
        overwrite = FALSE
      )
    )
  })
})

test_that("check_file_overwrite removes an existing file when overwrite is TRUE", {
  withr::with_tempdir({
    writeLines("test text", "test-text.txt")

    expect_message(
      check_file_overwrite("test-text.txt", overwrite = TRUE),
      "Removing existing"
    )
    expect_false(file.exists("test-text.txt"))
  })
})

test_that("check_file_overwrite is silent when the file does not exist", {
  withr::with_tempdir({
    expect_no_message(
      check_file_overwrite("missing.txt", overwrite = TRUE)
    )
    expect_null(check_file_overwrite("missing.txt", overwrite = TRUE))
  })
})

test_that("check_file_overwrite respects quiet", {
  withr::with_tempdir({
    writeLines("test text", "test-text.txt")

    expect_no_message(
      check_file_overwrite(
        "test-text.txt",
        ask = FALSE,
        overwrite = FALSE,
        quiet = TRUE
      )
    )
  })
})

test_that("check_path_fileext validates file extensions", {
  expect_error(check_path_fileext("file"))
  expect_no_error(check_path_fileext("file.csv"))
  expect_no_error(check_path_fileext("file.csv", fileext = "csv"))
  expect_error(check_path_fileext("file.csv", fileext = "png"))
  expect_error(check_path_fileext(c("a.csv", "b.png"), fileext = "csv"))
})

test_that("check_file_overwrite works with a directory path and filename", {
  withr::with_tempdir({
    dir.create("subdir")
    writeLines("test text", file.path("subdir", "test-text.txt"))

    expect_message(
      check_file_overwrite(
        filename = "test-text.txt",
        path = "subdir",
        overwrite = TRUE
      ),
      "Removing existing"
    )
    expect_false(file.exists(file.path("subdir", "test-text.txt")))
  })
})

test_that("check_file_overwrite works when path alone includes the file extension", {
  withr::with_tempdir({
    writeLines("test text", "test-text.txt")

    expect_message(
      check_file_overwrite(
        path = "test-text.txt",
        overwrite = TRUE
      ),
      "Removing existing"
    )
    expect_false(file.exists("test-text.txt"))
  })
})

test_that("check_file_overwrite asks interactively and respects the answer", {
  withr::with_tempdir({
    writeLines("test text", "test-text.txt")

    local_mocked_bindings(is_interactive = function() TRUE)
    local_mocked_bindings(cli_yesno = function(...) FALSE, .package = "cliExtras")

    expect_message(
      check_file_overwrite("test-text.txt", overwrite = FALSE, ask = TRUE),
      "can't be saved"
    )
    expect_true(file.exists("test-text.txt"))
  })

  withr::with_tempdir({
    writeLines("test text", "test-text.txt")

    local_mocked_bindings(is_interactive = function() TRUE)
    local_mocked_bindings(cli_yesno = function(...) TRUE, .package = "cliExtras")

    expect_message(
      check_file_overwrite("test-text.txt", overwrite = FALSE, ask = TRUE),
      "Removing existing"
    )
    expect_false(file.exists("test-text.txt"))
  })
})

test_that("check_path_fileext uses a custom message", {
  expect_error(
    check_path_fileext(
      "file",
      message = "custom message about {.arg {arg}}"
    ),
    "custom message about"
  )
})
