context("dq_drawer / dq_drawer")

ids <- list(NULL, "string", 2, TRUE, c("my", "id", "is"), list("my", "id", "is"))

test_that("dq_drawer works with any input", {
  expect_silent(dq_drawer())
  expect_silent(dq_drawer(NULL))
  expect_silent(dq_drawer(NULL, NULL))
  expect_silent(dq_drawer(NULL, test = NULL))
  expect_silent(dq_drawer(test = "test"))
})

test_that("dq_drawer works with any combination of names", {
  expect_null(dq_drawer("id", list("content")))
  expect_true(grepl("dqdrawer-c", toString(dq_drawer("id", c = list("content")))))
  expect_true(grepl("dqdrawer-c", toString(dq_drawer("id", c = "content", "content2"))))
  expect_true(grepl("dqdrawer-c", toString(dq_drawer("id", "content", c = "content2"))))
  expect_true(grepl("dqdrawer-c", toString(dq_drawer("id", list("content" = "content2")))))
})

context("dq_drawer shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/dq_drawer"))
})
