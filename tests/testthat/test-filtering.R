context("filtering / get_filters")

test_that("get_filters works with NULL inputs", {
  expect_silent(get_filters(NULL, NULL))
  expect_silent(get_filters(list("input1", "input2"), NULL))
  expect_silent(get_filters(NULL, "context"))
})

input <- readRDS(file.path("data", "shinySession.RData"))$input
test_that("get_filters doesn't work outside shiny reactive expressions", {
  expect_error(get_filters(input, NULL), "reactive expression")
})

test_that("get_filters works with correct input element", {
  shiny::isolate({
    expect_length(get_filters(input, NULL), 4)
    expect_length(get_filters(input, "rand"), 4)
    expect_silent(get_filters(input, "context"))
  })
})

context("filtering / text_filter")

test_that("text_filter works with null inputs", {
  expect_equal(text_filter(NULL, NULL), NULL)
  expect_equal(text_filter(NULL, ""), NULL)
  expect_equal(text_filter(NULL, "a"), NULL)
  expect_equal(text_filter(NULL, letters), NULL)
})

test_that("text_filter works properly and ignores cases", {
  df <- data.frame(letters, LETTERS, stringsAsFactors = F)
  expect_equal(text_filter(df, NULL), df)
  expect_equal(text_filter(df, ""), df)
  expect_equal(text_filter(df, c("a", "")), df[1, ])
  expect_equal(text_filter(df, c("a", "a")), df[1, ])
})

test_that("text_filter works with named filter values", {
  df <- data.frame(A = letters, B = sort(LETTERS, decreasing = TRUE), stringsAsFactors = F)
  expect_equal(text_filter(df, c(A = "a")), df[1, ])
  expect_equal(text_filter(df, c(B = "a")), df[26, ])
  expect_equal(text_filter(df, c(A = "a", B = "h")), df[NULL, ])
  expect_equal(text_filter(df, c(C = "c")), df)
})

test_that("text_filter works with NAs in data.frame", {
  inp <- data.frame(A = c("Value", NA), B = c("Empty", NA), stringsAsFactors = F)
  res <- data.frame(A = "Value", B = "Empty", stringsAsFactors = F)
  expect_equal(text_filter(inp, c("V", "")), res)
  inp <- data.frame(A = c("Value", NA), B = c(NA, "Empty"), stringsAsFactors = F)
  res <- data.frame(A = "Value", B = as.character(NA), stringsAsFactors = F)
  expect_equal(text_filter(inp, c("V", "")), res)
})

context("filtering / range_filter")

test_that("null works", {
  expect_equal(range_filter(NULL, NULL), NULL)
  expect_equal(range_filter(NULL, ""), NULL)
  expect_equal(range_filter(NULL, "a"), NULL)
  expect_equal(range_filter(NULL, letters), NULL)
  expect_equal(range_filter(NULL, list(a = 2)), NULL)
})

df <- data.frame(A = 1:50, B = 201:250)
test_that("wrong filters return full data frame", {
  expect_equal(range_filter(df, NULL), df)
  expect_equal(range_filter(df, ""), df)
  expect_equal(range_filter(df, c("a", "")), df)
  expect_equal(range_filter(df, c("a", T)), df)
})

test_that("correct filters work, named and unnamed", {
  expect_equal(range_filter(df, list(NA, NA)), df)
  expect_equal(range_filter(df, list(c(17, 25))), df[17:25,])
  expect_equal(range_filter(df, list(c(17, 25), NA)), df[17:25,])
  expect_equal(range_filter(df, list(NA, c(201, 220))), df[1:20,])
  expect_equal(range_filter(df, list(c(17, 25), c(201, 220))), df[17:20,])
  expect_equal(range_filter(df, list(A = c(17, 25))), df[17:25,])
  expect_equal(range_filter(df, list(B = c(201, 220))), df[1:20,])
})

test_that("correct date ranges work, named and unnamed", {
  df$B <- Sys.Date() - 1:50L
  start <- Sys.Date() - 12L
  end <- Sys.Date() - 3L
  expect_equal(range_filter(df, list(NA, c(start, end))), df[3:12,])
  expect_equal(range_filter(df, list(B = c(start, end))), df[3:12,])
})

test_that("correct filters work, even if not listed", {
  expect_equal(range_filter(df, c(NA, NA)), df)
  expect_equal(range_filter(df, c(17, 25)), df[17:25,])
})

test_that("not numeric ranges won't break the game", {
  expect_equal(range_filter(df, c(T, F)), df[NULL,]) # as.numeric(T) == 1
  expect_equal(range_filter(df, list(c("no", "yes"))), df)
  expect_equal(range_filter(df, list(c("no", "yes"), c(F, T))), df[NULL,])
  expect_equal(range_filter(df, list(B = c("201", "205"), A = c(F, T))), df[1,])
})

df <- data.frame(A = letters, B = LETTERS, stringsAsFactors = F)
test_that("not numeric values won't break the game", {
  expect_equal(range_filter(df, c(T, F)), df[NULL,]) # as.numeric(T) == 1
  expect_equal(range_filter(df, list(c("no", "yes"))), df)
  expect_equal(range_filter(df, list(c(1, 17))), df[NULL,])
})

test_that("NA in data.frame works", {
  inp <- data.frame(A = c(7, NA), B = c(9, NA))
  res <- data.frame(A = 7, B = 9)
  expect_equal(range_filter(inp, c(1, 200)), res)
  inp <- data.frame(A = c(7, NA), B = c(NA, 9))
  res <- data.frame(A = 7, B = NA_real_)
  expect_equal(range_filter(inp, c(1, 200)), res)
})


context("filtering / update_filters")

session <- readRDS(file.path("data", "shinySession.RData"))
test_that("update_page works with NULL inputs", {
  expect_null(update_filters(mtcars, NULL, NULL, NULL))
  expect_null(update_filters(NULL, "test", "context", session))
  expect_null(update_filters(mtcars, "test", 1, NULL))
})

test_that("update_filters works with proper inputs", {
  df <- data.frame(A = character(0), B = logical(0),C = as.Date(character(0)),
                   D = numeric(0), stringsAsFactors = F)
  expect_null(update_filters(df, "T", "rand", session))
  expect_null(update_filters(df, c("T", "S"), "rand", session))
  expect_null(update_filters(df, c("T", "R", "S", "Test", "wrong"), "rand", session))
})
