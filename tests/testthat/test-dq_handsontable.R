context("Test dq_handsontable_output in dq_handsontable.R")

test_that("dq_handsontable_output works with any input", {
  expect_null(dqshiny:::dq_handsontable_output(NULL))
  expect_silent(dqshiny:::dq_handsontable_output("string"))
  expect_silent(dqshiny:::dq_handsontable_output(2))
  expect_silent(dqshiny:::dq_handsontable_output(TRUE))
})

context("Test dq_render_handsontable in dq_handsontable.R")

test_that("dq_render_handsontable works with null input", {
  expect_null(dqshiny:::dq_render_handsontable(NULL, NULL, NULL))
  expect_null(dqshiny:::dq_render_handsontable(NULL, mtcars, NULL))
  expect_null(dqshiny:::dq_render_handsontable("test", NULL, "test"))
  expect_null(dqshiny:::dq_render_handsontable("test", mtcars, NULL))
})

test_that("dq_render_handsontable doesn't crash without a shiny session", {
  expect_silent(dqshiny:::dq_render_handsontable("test", mtcars, "test"))
})

test_that("dq_render_handsontable works properly with shiny session", {
  session <- readRDS(file.path("data", "shinySession.RData"))
  expect_equal(dqshiny:::dq_render_handsontable("test", mtcars, "test", session = session), mtcars)
  expect_equal(dqshiny:::dq_render_handsontable("test", mtcars, "test", filters = c("T", "S", "R", "R"), session = session), mtcars)
  expect_equal(dqshiny:::dq_render_handsontable("test", mtcars, "test", filters = "R", paged = FALSE, session = session), mtcars)
  expect_equal(dqshiny:::dq_render_handsontable("test", mtcars, "test", paged = TRUE, pageSize = 17, session = session), mtcars)
  expect_equal(dqshiny:::dq_render_handsontable("test", mtcars, "test", paged = TRUE, pageSize = 17, session = session), mtcars)
})

context("Test update_page in dq_handsontable.R")

session <- readRDS(file.path("data", "shinySession.RData"))
test_that("update_page works with NULL inputs", {
  expect_null(dqshiny:::update_page(NULL, NULL, NULL, NULL, NULL))
  expect_null(dqshiny:::update_page(mtcars, NULL, NULL, NULL, NULL))
  expect_null(dqshiny:::update_page(NULL, "test", 1, 15, session))
  expect_null(dqshiny:::update_page(mtcars, "test", 1, 15, NULL))
})

test_that("update_page works with proper inputs", {
  expect_equal(dqshiny:::update_page(mtcars, NULL, NULL, NULL, session), mtcars[1:25, ])
  expect_equal(dqshiny:::update_page(mtcars, NULL, 2, NULL, session), mtcars[26:nrow(mtcars), ])
  expect_equal(dqshiny:::update_page(mtcars, NULL, 2, 9, session), mtcars[10:18, ])
})

test_that("update_page works with strange inputs", {
  expect_equal(dqshiny:::update_page(mtcars, NULL, 1, 200, session), mtcars)
  expect_equal(dqshiny:::update_page(mtcars, NULL, 0, 200, session), mtcars)
  expect_equal(dqshiny:::update_page(mtcars, NULL, -2, 9, session), mtcars[1:9, ])
  expect_equal(dqshiny:::update_page(mtcars, NULL, NA, NA, session), mtcars[1:25, ])
  expect_equal(dqshiny:::update_page(mtcars, NULL, NA, 9, session), mtcars[1:9, ])
  expect_equal(dqshiny:::update_page(mtcars, NULL, 33, 25, session), mtcars[26:nrow(mtcars), ])
  expect_equal(dqshiny:::update_page(mtcars, NULL, Inf, 1, session), mtcars[nrow(mtcars), ])
  expect_equal(dqshiny:::update_page(mtcars, NULL, 1, Inf, session), mtcars)
  expect_equal(dqshiny:::update_page(mtcars, NULL, Inf, Inf, session), mtcars)
})
