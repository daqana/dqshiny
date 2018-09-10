context("Test reset_slider_input in reset_slider_input.R")

test_that("reset_slider_input works with all inputs", {
  expect_null(reset_slider_input(NULL))
  expect_null(reset_slider_input("string"))
  expect_null(reset_slider_input(2))
  expect_null(reset_slider_input(TRUE))
})
