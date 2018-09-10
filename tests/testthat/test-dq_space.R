context("Test dq_space in dq_space.R")

test_that("dq_space works with all inputs", {
  expect_equal(dq_space(NULL), shiny::tags$div(style = "height:;clear:both;", dq_dep))
  expect_equal(dq_space(20), shiny::tags$div(style = "height:20px;clear:both;", dq_dep))
  expect_equal(dq_space("20px"), shiny::tags$div(style = "height:20px;clear:both;", dq_dep))
  expect_equal(dq_space("20%"), shiny::tags$div(style = "height:20%;clear:both;", dq_dep))
  expect_equal(dq_space("20px", NULL), shiny::tags$div(style = "height:20px;clear:both;", dq_dep))
  expect_equal(dq_space("20", "left"), shiny::tags$div(style = "height:20px;clear:left;", dq_dep))
  expect_equal(dq_space("20", TRUE), shiny::tags$div(style = "height:20px;clear:both;", dq_dep))
})
