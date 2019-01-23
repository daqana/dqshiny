context("dq_handsontable / dq_handsontable_output")

test_that("dq_handsontable_output works with any input", {
  expect_null(dq_handsontable_output(NULL))
  expect_silent(dq_handsontable_output("string"))
  expect_silent(dq_handsontable_output(2))
  expect_silent(dq_handsontable_output(TRUE))
})

context("dq_handsontable / dq_render_handsontable")

test_that("dq_render_handsontable works with bad input", {
  expect_silent(dq_render_handsontable(NULL, NULL))
  expect_silent(dq_render_handsontable(NULL, mtcars))
  expect_silent(dq_render_handsontable("test", NULL))
  expect_silent(dq_render_handsontable("test", mtcars))
})

test_that("dq_render_handsontable works properly with different parameters", {
  df <- data.frame(A = character(0), B = logical(0), C = as.Date(character(0)),
                   D = numeric(0), stringsAsFactors = FALSE)

  session <- create_test_session(
    "id",
    list("id" = "myTest"),
    list("id-filters" = "test", "id-pages" = "test"))

  session$makeScope <- function(id) {
    s <- session
    pre <- paste0(id, "-")
    if (length(s$input) > 0L) {
      s$input <- s$input[startsWith(names(s$input), pre)]
      names(s$input) <- gsub(pre, "", names(s$input), fixed = TRUE)
    }
    if (length(s$output) > 0L) {
      s$output <- s$output[startsWith(names(s$output), pre)]
      names(s$output) <- gsub(pre, "", names(s$output), fixed = TRUE)
    }
    s
  }

  expect_silent(dq_render_handsontable("id", df, session = session))
  expect_warning(dq_render_handsontable("id", df, context = "con", session = session), "deprecated")
  expect_silent(dq_render_handsontable("id", df, filters = NULL, session = session))
  expect_silent(dq_render_handsontable("id", df, page_size = 78L, session = session))
  expect_silent(dq_render_handsontable("id", df, page_size = c(78L, 1000L), session = session))
  expect_silent(dq_render_handsontable("id", df, reset = FALSE, session = session))
  expect_silent(dq_render_handsontable("id", df, sorting = TRUE, session = session))
  expect_silent(dq_render_handsontable("id", df, sorting = TRUE, reset = TRUE, session = session))
  expect_silent(dq_render_handsontable("id", df, width_align = TRUE, session = session))
  expect_silent(dq_render_handsontable("id", df, horizontal_scroll = TRUE, session = session))
  expect_silent(dq_render_handsontable("id", df, table_param = list(readOnly = TRUE), session = session))
  expect_silent(dq_render_handsontable("id", df, cols_param = list(colWidths = 25), session = session))
  expect_silent(dq_render_handsontable("id", df, col_param = list(1, type = "text"), session = session))
  expect_silent(dq_render_handsontable("id", df, session = session, cell_param = list(
    row = 1, col = 1, type = "text")
  ))
  rV <- shiny::reactiveValues(id = df)
  expect_silent(dq_render_handsontable("id", rV, page_size = 78L, session = session))
  rV <- shiny::reactive(df)
  expect_silent(dq_render_handsontable("id", rV, session = session))
})

context("dq_handsontable / shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/dq_handsontable"))
})
