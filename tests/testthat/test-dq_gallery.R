context("dq_gallery / dq_gallery")

ids <- list(NULL, "string", 2, TRUE, c("my", "id", "is"), list("my", "id", "is"))

test_that("dq_gallery works with any input", {
  expect_silent(dq_gallery())
  expect_silent(dq_gallery(NULL))
  expect_silent(dq_gallery(NULL, NULL))
  expect_silent(dq_gallery(NULL, test = NULL))
  expect_silent(dq_gallery(test = "test"))
})

test_that("dq_gallery works with any combination of names", {
  expect_true(grepl("dqgallery-c", toString(dq_gallery("id", c = list("content")))))
  expect_true(grepl("dqgallery-c", toString(dq_gallery("id", c = "content", "content2"))))
  expect_true(grepl("dqgallery-c", toString(dq_gallery("id", "content", c = "content2"))))
})

context("dq_gallery shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/dq_gallery"))
})
