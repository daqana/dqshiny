context("dq_handsontable / dq_handsontable_output")

test_that("dq_handsontable_output works with any input", {
  expect_null(dq_handsontable_output(NULL))
  expect_silent(dq_handsontable_output("string"))
  expect_silent(dq_handsontable_output(2))
  expect_silent(dq_handsontable_output(TRUE))
})

context("dq_handsontable / dq_render_handsontable")

test_that("dq_render_handsontable works with null input", {
  expect_null(dq_render_handsontable(NULL, NULL))
  expect_null(dq_render_handsontable(NULL, mtcars))
  expect_null(dq_render_handsontable("test", NULL))
})

test_that("dq_render_handsontable doesn't crash without a shiny session", {
  expect_silent(dq_render_handsontable("test", mtcars, "test"))
})

test_that("dq_render_handsontable works properly with different parameters", {
  df <- data.frame(A = character(0), B = logical(0), C = as.Date(character(0)),
                   D = numeric(0), stringsAsFactors = FALSE)
  expect_silent(dq_render_handsontable("id", df))
  expect_silent(dq_render_handsontable("id", df, context = "context"))
  expect_silent(dq_render_handsontable("id", df, filters = NULL))
  expect_silent(dq_render_handsontable("id", df, page_size = 78L))
  expect_silent(dq_render_handsontable("id", df, page_size = c(78L, 1000L)))
  expect_silent(dq_render_handsontable("id", df, reset = FALSE))
  expect_silent(dq_render_handsontable("id", df, sorting = TRUE))
  expect_silent(dq_render_handsontable("id", df, sorting = TRUE, reset = TRUE))
  expect_silent(dq_render_handsontable("id", df, width_align = TRUE))
  expect_silent(dq_render_handsontable("id", df, horizontal_scroll = TRUE))
  expect_silent(dq_render_handsontable("id", df, table_param = list(readOnly = TRUE)))
  expect_silent(dq_render_handsontable("id", df, cols_param = list(colWidths = 25)))
  expect_silent(dq_render_handsontable("id", df, col_param = list(1, type = "text")))
  expect_silent(dq_render_handsontable("id", df, cell_param = list(
    row = 1, col = 1, type = "text")
  ))
  rV <- shiny::reactiveValues(id = df)
  expect_silent(dq_render_handsontable("id", rV, page_size = 78L))
})

context("dq_handsontable / shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/dq_handsontable"))
})
