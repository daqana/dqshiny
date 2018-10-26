context("package quality")

skip_if(!requireNamespace("dqutils"))

test_that("Test list of functions in this test file", {
  name <- "test-package_quality.R"
  full <- dqutils::get_package_functions(name, expand = TRUE)
  expect_equal(full$packages, c("dqutils", "testthat"))
})

test_that("Test that testthat is contained in DESCRIPTION file", {
  pkgs <- dqutils::get_package_description("dqshiny")
  pkgs <- unname(unlist(pkgs))
  expect_true(is.element("testthat", pkgs))
})

test_that("Test if all used packages are listed in DESCRIPTION", {
  full <- dqutils::get_package_functions("../../R")$package
  pkgs <- dqutils::get_package_description("dqshiny")
  pkgs <- unname(unlist(pkgs))
  sdiff <- setdiff(full, pkgs)
  expect_true(length(sdiff) == 0, info = sdiff)
})
