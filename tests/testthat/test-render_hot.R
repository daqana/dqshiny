context("render_hot / render_hot")

test_that("render_hot works with all inputs", {
  expect_null(render_hot(NULL))
  expect_null(render_hot("string"))
  expect_null(render_hot(2))
  expect_null(render_hot(TRUE))
})
