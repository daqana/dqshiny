context("state / enable")

test_that("enable works with any input", {
  expect_null(enable(NULL))
  expect_null(enable("string"))
  expect_null(enable(2))
  expect_null(enable(TRUE))
  expect_null(enable(c("my", "id", "is")))
  expect_null(enable(list("my", "id", "is")))
})

context("state / disable")

test_that("disable works with any input", {
  expect_null(disable(NULL))
  expect_null(disable("string"))
  expect_null(disable(2))
  expect_null(disable(TRUE))
  expect_null(disable(c("my", "id", "is")))
  expect_null(disable(list("my", "id", "is")))
})

context("state / toggle_state")

test_that("toggle_state works with any input", {
  expect_null(toggle_state(NULL))
  expect_null(toggle_state("string"))
  expect_null(toggle_state(2))
  expect_null(toggle_state(TRUE))
  expect_null(toggle_state(c("my", "id", "is")))
  expect_null(toggle_state(list("my", "id", "is")))
})

test_that("toggle_state works with conditions", {
  expect_null(toggle_state("id", NULL))
  expect_null(toggle_state("id", TRUE))
  expect_null(toggle_state("id", FALSE))
})

context("state / shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/state"))
})
