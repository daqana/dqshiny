context("reset_slider_input / reset_slider_input")

test_that("reset_slider_input works with all inputs", {
  expect_null(reset_slider_input(NULL))
  expect_null(reset_slider_input("string"))
  expect_null(reset_slider_input(2))
  expect_null(reset_slider_input(TRUE))
})
