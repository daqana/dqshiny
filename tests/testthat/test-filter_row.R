context("Test filter_row in filter_row.R")

test_that("null works", {
  expect_silent(res <- shiny::fluidRow(class = "filter-row"))
  expect_equal(dqshiny:::filter_row(NULL, NULL), res)
  expect_equal(dqshiny:::filter_row(NULL, NULL, reset = FALSE), res)
})

test_that("all filters work for empty data", {
  df <- data.frame(A = character(0), B = logical(0), C = as.Date(character(0)),
                   D = numeric(0), stringsAsFactors = FALSE)
  expect_silent(dqshiny:::filter_row(NULL, df))
  expect_silent(dqshiny:::filter_row(NULL, df, reset = FALSE))
  expect_silent(dqshiny:::filter_row(NULL, df, "S"))
  expect_silent(dqshiny:::filter_row(NULL, df, "R"))
  expect_silent(dqshiny:::filter_row(NULL, df, ""))
})

test_that("all filters work for proper data", {
  df <- data.frame(A = rep("hello", 20), B = rep(c("huhu", "haha"), 10),
                   C = 1:20, D = Sys.Date() - 0:19, stringsAsFactors = FALSE)
  expect_silent(dqshiny:::filter_row(NULL, df))
  expect_silent(dqshiny:::filter_row(NULL, df, reset = FALSE))
  expect_silent(dqshiny:::filter_row(NULL, df, sorting = TRUE))
  expect_silent(dqshiny:::filter_row(NULL, df, "s"))
  expect_silent(dqshiny:::filter_row(NULL, df, "R"))
  expect_silent(dqshiny:::filter_row(NULL, df, ""))
  expect_silent(dqshiny:::filter_row(NULL, df, c("T", "S")))
  expect_silent(dqshiny:::filter_row(NULL, df, c("t", "s", "r")))
  expect_silent(dqshiny:::filter_row(NULL, df, c("T", "S", "r", "T")))
  expect_silent(dqshiny:::filter_row(NULL, df, c("T", "s", "R", "", "R")))
  expect_silent(dqshiny:::filter_row(NULL, df, c("T", NA, "NA", NULL)))
  expect_silent(dqshiny:::filter_row(NULL, df, c(NA, "something", "strange", "")))
})

test_that("all filters work for proper data with non bootstrap width", {
  df <- mtcars
  expect_silent(dqshiny:::filter_row(NULL, df))
  expect_silent(dqshiny:::filter_row(NULL, df, reset = FALSE))
  expect_silent(dqshiny:::filter_row(NULL, df, sorting = TRUE))
  expect_silent(dqshiny:::filter_row(NULL, df, "s"))
  expect_silent(dqshiny:::filter_row(NULL, df, "R"))
  expect_silent(dqshiny:::filter_row(NULL, df, ""))
  expect_silent(dqshiny:::filter_row(NULL, df, c("T", "S")))
  expect_silent(dqshiny:::filter_row(NULL, df, c("t", "s", "r")))
  expect_silent(dqshiny:::filter_row(NULL, df, c("T", "S", "r", "T")))
  expect_silent(dqshiny:::filter_row(NULL, df, c("T", "s", "R", "", "R")))
  expect_silent(dqshiny:::filter_row(NULL, df, c("T", NA, "NA", NULL)))
  expect_silent(dqshiny:::filter_row(NULL, df, c(NA, "something", "strange", "")))
})
