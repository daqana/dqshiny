context("dq_popover / dq_pop")

test_that("bad inputs work", {
  expect_silent(dq_pop(NULL, NULL, NULL))
})

test_that("proper inputs work", {
  expect_silent(dq_pop(shiny::div(), "Title", "Content"))
  expect_silent(dq_pop(shiny::actionButton("test", "test"), "Title", "Content"))
  expect_silent(dq_pop(shiny::actionButton("test", "test"), "Title", "Content", trigger = "manual"))
})
