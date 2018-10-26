context("click / click")

test_that("works with all inputs", {
  expect_silent(click(NULL))
  expect_silent(click("string"))
  expect_silent(click(2))
  expect_silent(click(TRUE))
})
