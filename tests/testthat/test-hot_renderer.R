context("Test dq_hot_date_renderer in hot_renderer.R")

test_that("dq_hot_date_renderer works", {
  expect_silent(dq_hot_date_renderer())
})

context("Test dq_hot_empty_renderer in hot_renderer.R")

test_that("dq_hot_empty_renderer works with any input", {
  expect_silent(dq_hot_empty_renderer(NULL))
  expect_silent(dq_hot_empty_renderer("string"))
  expect_silent(dq_hot_empty_renderer(2))
  expect_silent(dq_hot_empty_renderer(TRUE))
  expect_silent(dq_hot_empty_renderer(c("my", "id", "is")))
  expect_silent(dq_hot_empty_renderer(list("my", "id", "is")))
})

context("Test dq_hot_html_renderer in hot_renderer.R")

test_that("dq_hot_html_renderer works", {
  expect_silent(dq_hot_html_renderer())
})
