context("sorting / sort_button")

test_that("bad inputs works", {
  expect_silent(sort_button(paste0, NULL, NULL))
})

test_that("all parameters work", {
  expect_true(grepl("\"sort\"", sort_button(paste0, NULL, NULL)))
  expect_true(grepl("\"sortname\"", sort_button(paste0, "name", NULL)))
})

context("sorting / sort_data")

test_that("bad inputs works", {
  expect_silent(sort_data(NULL, NULL, NULL))
})

test_that("all parameters work", {
  df <- data.frame(A = sample(1:100, 100), B = sample(1:100, 100))
  expect_silent(sort_data(df, 1, NULL))
  expect_silent(sort_data(df, NULL, 2))
  df <- sort_data(df, "up", 2)
  expect_equal(order(df[[2]]), 1:100)
  df <- sort_data(df, "down", 1)
  expect_equal(order(df[[1]]), 100:1)
  df <- sort_data(df, NULL, NULL)
  expect_equal(order(as.integer(rownames(df))), 1:100)
})

test_that("none numeric row names work", {
  df <- data.frame(A = sample(1:100, 100), B = sample(1:100, 100))
  rownames(df) <- paste0("A", 1:100, "B")
  expect_silent(sort_data(df, NULL, NULL))
})
