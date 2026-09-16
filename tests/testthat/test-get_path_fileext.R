test_that("get_path_fileext returns fileext as-is when supplied", {
  expect_identical(
    get_path_fileext("any/path", fileext = "csv"),
    "csv"
  )
})

test_that("get_path_fileext extracts a single extension from a directory", {
  withr::with_tempdir({
    writeLines("a", "a.txt")
    writeLines("b", "b.txt")

    expect_identical(get_path_fileext("."), "txt")
  })
})

test_that("get_path_fileext extracts extension from a single file", {
  withr::with_tempdir({
    writeLines("a", "a.txt")

    expect_identical(get_path_fileext("a.txt"), "txt")
  })
})

test_that("get_path_fileext messages and returns n most common extensions", {
  withr::with_tempdir({
    writeLines("a", "a.txt")
    writeLines("b", "b.txt")
    writeLines("c", "c.csv")

    expect_message(
      result <- get_path_fileext(".", n = 1),
      "more than"
    )
    expect_identical(result, "txt")
  })
})

test_that("get_path_fileext respects quiet", {
  withr::with_tempdir({
    writeLines("a", "a.txt")
    writeLines("b", "b.txt")
    writeLines("c", "c.csv")

    expect_no_message(get_path_fileext(".", n = 1, quiet = TRUE))
  })
})

test_that("list_path_fileext lists extensions for files at a directory", {
  withr::with_tempdir({
    writeLines("a", "a.txt")
    writeLines("b", "b.csv")

    expect_setequal(unique(list_path_fileext(".")), c("txt", "csv"))
  })
})

test_that("list_path_fileext lists the extension for a single file", {
  withr::with_tempdir({
    writeLines("a", "a.txt")

    expect_identical(list_path_fileext("a.txt"), "txt")
  })
})

test_that("list_path_fileext errors for a missing or nonexistent path", {
  expect_error(list_path_fileext(NULL))
  expect_error(list_path_fileext("path/does/not/exist"))
})

test_that("list_path_fileext handles empty directories with allow_null", {
  withr::with_tempdir({
    dir.create("empty-dir")

    expect_error(list_path_fileext("empty-dir"))
    expect_null(list_path_fileext("empty-dir", allow_null = TRUE))
  })
})
