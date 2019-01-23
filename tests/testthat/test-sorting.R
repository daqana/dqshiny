context("sorting / sort_button")

test_that("bad inputs work", {
  expect_silent(sort_button(paste0, NULL, NULL))
})

test_that("all parameters work", {
  expect_true(grepl("\"sort\"", sort_button(paste0, NULL, NULL)))
  expect_true(grepl("\"sortname\"", sort_button(paste0, "name", NULL)))
})

context("sorting / sort_data")

test_that("bad inputs work", {
  expect_silent(sort_data(NULL, NULL))
})

test_that("all parameters work", {
  df <- data.frame(A = sample(1:100, 100), B = sample(1:100, 100))
  expect_silent(sort_data(df, 1))
  expect_silent(sort_data(df, NULL))
  df <- sort_data(df, list(dir = "up", col = 2))
  expect_equal(order(df[[2]]), 1:100)
  df <- sort_data(df, list(dir = "down", col = 1))
  expect_equal(order(df[[1]]), 100:1)
  df <- sort_data(df, NULL)
  expect_equal(order(as.integer(rownames(df))), 1:100)
})

test_that("none numeric row names work", {
  df <- data.frame(A = sample(1:100, 100), B = sample(1:100, 100))
  rownames(df) <- paste0("A", 1:100, "B")
  expect_silent(sort_data(df, NULL))
})

context("sorting / check_sorting")

test_that("bad inputs work", {
  expect_null(check_sorting(NULL, NULL, NULL))
  expect_null(check_sorting(NULL, FALSE, NULL))
  expect_null(check_sorting(list(dir = "", col = ""), NULL, 1:5))
  expect_null(check_sorting(list(dir = "", col = ""), "test", 1:5))
})

test_that("incorrect inputs result in default sorting", {
  res <- list(dir = "", col = "")
  expect_equal(check_sorting(NULL, TRUE, NULL), res)
  expect_equal(check_sorting(NULL, TRUE, 1:5), res)
  expect_equal(check_sorting(res, TRUE, 1:5), res)
  expect_equal(check_sorting(list(dir = "", col = "test"), TRUE, 1:5), res)
  expect_equal(check_sorting(list(dir = "test", col = ""), TRUE, 1:5), res)
  expect_equal(check_sorting(list(dir = "test", col = 7), TRUE, 1:5), res)
})

test_that("proper inputs result in expected sorting", {
  res <- list(dir = "up", col = "b")
  expect_equal(check_sorting(list(dir = "up", col = 2), TRUE, letters[1:5]), res)
  expect_equal(check_sorting(c(dir = "up", col = "b"), TRUE, letters[1:5]), res)
  expect_equal(check_sorting(res, TRUE, letters[1:5]), res)
})
