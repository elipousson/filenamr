test_that("str_affix adds a prefix and postfix", {
  expect_identical(
    str_affix(string = "My File", prefix = "source", postfix = "final"),
    "source_my_file_final"
  )
})

test_that("str_affix works with only a prefix or only a postfix", {
  expect_identical(str_affix(string = "My File", prefix = "source"), "source_my_file")
  expect_identical(str_affix(string = "My File", postfix = "final"), "my_file_final")
})

test_that("str_affix supports multi-value prefix and postfix", {
  expect_identical(
    str_affix(string = "My File", prefix = c("a", "b")),
    "a_b_my_file"
  )
  expect_identical(
    str_affix(string = "My File", postfix = c("a", "b")),
    "my_file_a_b"
  )
})

test_that("str_affix returns character(0) when string is NULL", {
  expect_identical(str_affix(string = NULL), character(0))
})

test_that("str_affix pads digits when pad and width are supplied", {
  expect_identical(
    str_affix(string = "file1", pad = "0", width = 3),
    "file001"
  )
})

test_that("str_affix validates input types", {
  expect_error(str_affix(string = 1))
  expect_error(str_affix(string = "file", prefix = 1))
  expect_error(str_affix(string = "file", postfix = 1))
})

test_that("str_affix collapses repeated separators", {
  expect_identical(
    str_affix(string = "file", prefix = "a_", use_clean_names = FALSE, use_make_names = FALSE),
    "a_file"
  )
})

test_that("str_prefix adds a prefix or postfix to a string", {
  expect_identical(str_prefix("file", "prefix"), "prefix_file")
  expect_identical(str_prefix("file", "postfix", is_postfix = TRUE), "file_postfix")
})

test_that("str_prefix returns string unchanged if prefix is NULL", {
  expect_identical(str_prefix("file", NULL), "file")
})

test_that("str_prefix supports multi-value prefixes", {
  expect_identical(str_prefix("file", c("a", "b")), "a_b_file")
})

test_that("str_prefix expands date and time prefixes", {
  expect_identical(
    str_prefix("file", "date", date.format = "%Y-%m-%d"),
    paste0(format(Sys.Date(), "%Y-%m-%d"), "_file")
  )

  expect_match(
    str_prefix("file", "time"),
    "^\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}_[ap]m_file$"
  )
})

test_that("str_prefix formats Date and POSIXct prefixes", {
  expect_identical(
    str_prefix("file", as.Date("2023-02-09")),
    "2023-02-09_file"
  )

  expect_identical(
    str_prefix("file", as.POSIXct("2023-02-09 09:00 AM")),
    "2023-02-09_09-00-00_am_file"
  )
})
