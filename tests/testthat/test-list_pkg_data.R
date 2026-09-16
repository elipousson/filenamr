test_that("list_pkg_datasets lists datasets for an installed package", {
  result <- list_pkg_datasets("datasets")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("item", "path", "package"))
  expect_true(all(result$package == "datasets"))
  expect_true(nrow(result) > 0)
})

test_that("list_pkg_extdata returns character(0) when no extdata files exist", {
  expect_identical(list_pkg_extdata("filenamr"), character(0))
})

test_that("list_pkg_extdata lists files in a package's extdata directory", {
  skip_if_not_installed("xml2")
  skip_if(identical(system.file("extdata", package = "xml2"), ""))

  result <- list_pkg_extdata("xml2")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("item", "path", "package"))
  expect_true(all(result$package == "xml2"))
  expect_true(nrow(result) > 0)
})

test_that("list_pkg_cachedata returns character(0) when cache directory is empty", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp, .package = "rappdirs")

  expect_identical(list_pkg_cachedata("anypkg"), character(0))
})

test_that("list_pkg_cachedata lists files in the package cache directory", {
  tmp <- withr::local_tempdir()
  writeLines("x", file.path(tmp, "sample.txt"))
  local_mocked_bindings(user_cache_dir = function(...) tmp, .package = "rappdirs")

  result <- list_pkg_cachedata("anypkg")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("item", "path", "package"))
  expect_identical(result$item, "sample")
  expect_identical(result$package, "anypkg")
})

test_that("list_pkg_data checks that pkg is installed", {
  expect_error(list_pkg_data(pkg = "not.a.real.package.xyz"))
})

test_that("list_pkg_data combines datasets, extdata, and cache data", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp, .package = "rappdirs")

  result <- list_pkg_data(pkg = "datasets")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("item", "path", "package"))
  expect_true(nrow(result) >= nrow(list_pkg_datasets("datasets")))
})
