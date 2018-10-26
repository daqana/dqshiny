context("display / hide")

test_that("hide works with any input", {
  expect_null(hide(NULL))
  expect_null(hide("string"))
  expect_null(hide(2))
  expect_null(hide(TRUE))
  expect_null(hide(c("my", "id", "is")))
  expect_null(hide(list("my", "id", "is")))
})

context("display / show")

test_that("show works with any input", {
  expect_null(show(NULL))
  expect_null(show("string"))
  expect_null(show(2))
  expect_null(show(TRUE))
  expect_null(show(c("my", "id", "is")))
  expect_null(show(list("my", "id", "is")))
})

context("display / toggle")

test_that("toggle works with any input", {
  expect_null(toggle(NULL))
  expect_null(toggle("string"))
  expect_null(toggle(2))
  expect_null(toggle(TRUE))
  expect_null(toggle(c("my", "id", "is")))
  expect_null(toggle(list("my", "id", "is")))
})

test_that("toggle works with conditions", {
  expect_null(toggle("id", NULL))
  expect_null(toggle("id", TRUE))
  expect_null(toggle("id", FALSE))
})

context("display shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/display"))
})
