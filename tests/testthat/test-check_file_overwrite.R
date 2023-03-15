test_that("check_file_overwrite works", {
  withr::with_tempdir({
    writeLines("test text", "test-text.txt")
    expect_error(
      check_file_overwrite(
        "test-text.txt",
        ask = FALSE,
        overwrite = FALSE
      )
    )
  })
})
