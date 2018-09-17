context("Test dq_handsontable_output in dq_handsontable.R")

test_that("dq_handsontable_output works with any input", {
  expect_null(dqshiny:::dq_handsontable_output(NULL))
  expect_silent(dqshiny:::dq_handsontable_output("string"))
  expect_silent(dqshiny:::dq_handsontable_output(2))
  expect_silent(dqshiny:::dq_handsontable_output(TRUE))
})

context("Test dq_render_handsontable in dq_handsontable.R")

test_that("dq_render_handsontable works with null input", {
  expect_null(dqshiny:::dq_render_handsontable(NULL, NULL))
  expect_null(dqshiny:::dq_render_handsontable(NULL, mtcars))
  expect_null(dqshiny:::dq_render_handsontable("test", NULL))
})

test_that("dq_render_handsontable doesn't crash without a shiny session", {
  expect_silent(dqshiny:::dq_render_handsontable("test", mtcars, "test"))
})

test_that("dq_render_handsontable works properly with different parameters", {
  df <- data.frame(A = character(0), B = logical(0), C = as.Date(character(0)),
                   D = numeric(0), stringsAsFactors = F)
  expect_silent(dqshiny:::dq_render_handsontable("id", df))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, context = "context"))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, filters = NULL))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, page_size = 78))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, reset = FALSE))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, sorting = TRUE))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, sorting = TRUE, reset = TRUE))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, width_align = TRUE))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, horizontal_scroll = TRUE))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, table_params = NULL))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, col_params = NULL))
  expect_silent(dqshiny:::dq_render_handsontable("id", df, col1 = list(type = "text")))
  rV <- shiny::reactiveValues(id = df)
  expect_silent(dqshiny:::dq_render_handsontable("id", rV, page_size = 78))
})
