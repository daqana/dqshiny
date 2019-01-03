context("filter_row / filter_row")

test_that("null works", {
  expect_silent(res <- shiny::fluidRow(class = "filter-row"))
  expect_equal(filter_row(NULL, NULL), res)
  expect_equal(filter_row(NULL, NULL, reset = FALSE), res)
})

test_that("all filters work for empty data", {
  df <- data.frame(A = character(), B = logical(), C = as.Date(character()),
                   D = numeric(), stringsAsFactors = FALSE)
  expect_silent(filter_row("con", list(con = df)))
  expect_silent(filter_row("con", list(con = df), reset = FALSE))
  expect_silent(filter_row("con", list(con = df), "S"))
  expect_silent(filter_row("con", list(con = df), "R"))
  expect_silent(filter_row("con", list(con = df), "A"))
  expect_silent(filter_row("con", list(con = df), "D"))
  expect_silent(filter_row("con", list(con = df), ""))
})

test_that("all filters work for proper data", {
  df <- data.frame(A = rep("hello", 20), B = rep(c("huhu", "haha"), 10),
                   C = 1:20, D = Sys.Date() - 0:19, stringsAsFactors = FALSE)
  expect_silent(filter_row("con", list(con = df)))
  expect_silent(filter_row("con", list(con = df), reset = FALSE))
  expect_silent(filter_row("con", list(con = df), sorting = TRUE))
  expect_silent(filter_row("con", list(con = df), "s"))
  expect_silent(filter_row("con", list(con = df), "R"))
  expect_silent(filter_row("con", list(con = df), ""))
  expect_silent(filter_row("con", list(con = df), c("T", "S")))
  expect_silent(filter_row("con", list(con = df), c("t", "s", "r")))
  expect_silent(filter_row("con", list(con = df), c("T", "A", "r", "T")))
  expect_silent(filter_row("con", list(con = df), c("T", "a", "R", "", "R")))
  expect_silent(filter_row("con", list(con = df), c("T", NA, "NA", NULL)))
  expect_silent(filter_row("con", list(con = df), c(NA, "something", "strange", "")))
})

test_that("all filters work for proper data with non bootstrap width", {
  df <- mtcars
  expect_silent(filter_row("con", list(con = df)))
  expect_silent(filter_row("con", list(con = df), reset = FALSE))
  expect_silent(filter_row("con", list(con = df), sorting = TRUE))
  expect_silent(filter_row("con", list(con = df), "s"))
  expect_silent(filter_row("con", list(con = df), "R"))
  expect_silent(filter_row("con", list(con = df), ""))
  expect_silent(filter_row("con", list(con = df), c("T", "S")))
  expect_silent(filter_row("con", list(con = df), c("t", "s", "r")))
  expect_silent(filter_row("con", list(con = df), c("T", "A", "r", "T")))
  expect_silent(filter_row("con", list(con = df), c("T", "a", "R", "", "R")))
  expect_silent(filter_row("con", list(con = df), c("T", NA, "NA", NULL)))
  expect_silent(filter_row("con", list(con = df), c(NA, "something", "strange", "")))
})

test_that("all sorting options work", {
  df <- mtcars
  expect_silent(filter_row("con", list(con = df)))
  expect_silent(filter_row("con", list(con = df), sorting = TRUE))
  expect_silent(filter_row("con", list(con = df), sorting = c(dir = "down", col = "mpg")))
})
