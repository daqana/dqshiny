context("init / init")

test_that("init works without error", {
  expect_length(init(), 1L)
})

context("init / init_fonts")

test_that("init_fonts works without error", {
  expect_silent(init_fonts("."))
  expect_silent(init_fonts("data", silent = TRUE))
  expect_warning(init_fonts("data", silent = FALSE), "badFont")
})
