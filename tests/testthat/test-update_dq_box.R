context("Test update_dq_box in dq_box.R")

test_that("works with all inputs", {
  expect_silent(update_dq_box(NULL, NULL))
  expect_silent(update_dq_box("string", TRUE))
  expect_silent(update_dq_box(2, FALSE))
  expect_silent(update_dq_box(TRUE, TRUE, TRUE))
})
