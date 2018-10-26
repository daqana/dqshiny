context("dq_space / dq_space")

test_that("dq_space works with all inputs", {
  dep <- init()
  expect_equal(dq_space(NULL), shiny::tags$div(style = "height:;clear:both;", dep))
  expect_equal(dq_space(20), shiny::tags$div(style = "height:20px;clear:both;", dep))
  expect_equal(dq_space("20px"), shiny::tags$div(style = "height:20px;clear:both;", dep))
  expect_equal(dq_space("20%"), shiny::tags$div(style = "height:20%;clear:both;", dep))
  expect_equal(dq_space("20px", NULL), shiny::tags$div(style = "height:20px;clear:both;", dep))
  expect_equal(dq_space("20", "left"), shiny::tags$div(style = "height:20px;clear:left;", dep))
  expect_equal(dq_space("20", TRUE), shiny::tags$div(style = "height:20px;clear:both;", dep))
})
