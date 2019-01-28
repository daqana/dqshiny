context("filter_row / filter_row")

test_that("null works", {
  expect_silent(res <- shiny::fluidRow(class = "filter-row"))
  expect_equal(filter_row(NULL, NULL, NULL, NULL, NULL), res)
  expect_equal(filter_row(NULL, NULL, NULL, NULL, NULL, reset = FALSE), res)
})

test_that("all filters work for empty data", {
  df <- data.frame(A = character(), B = logical(), C = as.Date(character()),
                   D = numeric(), stringsAsFactors = FALSE)
  l <- length(df)
  expect_silent(filter_row(paste0, list(full = df), rep("T", l), TRUE, NULL, reset = FALSE))
  expect_silent(filter_row(paste0, list(full = df), rep("T", l), TRUE, NULL))
  expect_silent(filter_row(paste0, list(full = df), rep("", l), TRUE, NULL))
})

test_that("all filters work for proper data", {
  df <- data.frame(A = rep("hello", 20), B = rep(c("huhu", "haha"), 10),
                   C = 1:20, D = Sys.Date() - 0:19, stringsAsFactors = FALSE)
  filts <- c("A", "S", "R", "D")
  expect_silent(filter_row(paste0, list(full = df), filts, TRUE, NULL, reset = FALSE))
  expect_silent(filter_row(paste0, list(full = df), "T", TRUE, list(dir = "", col = "")))
  expect_silent(filter_row(paste0, list(full = df), "", TRUE, NULL))
})

test_that("all filters work for proper data with non bootstrap width", {
  df <- mtcars
  expect_silent(filter_row(paste0, list(full = df), "T", TRUE, NULL))
  expect_silent(filter_row(paste0, list(full = df), "T", TRUE, list(dir = "", col = ""), reset = FALSE))
  expect_silent(filter_row(paste0, list(full = df), "T", TRUE, list(dir = "up", col = 1)))
  expect_silent(filter_row(paste0, list(full = df), "", 1, list(dir = "down", col = "mpg")))
})

test_that("all sorting options work", {
  df <- mtcars
  expect_silent(filter_row(paste0, list(full = df), "T", 2, NULL))
  expect_silent(filter_row(paste0, list(full = df), "T", "cyl", list(dir = "down", col = "mpg")))
  expect_silent(filter_row(paste0, list(full = df), "T", TRUE, list(dir = "down", col = "mpg")))
})

context("filter_row / correct_type")

test_that("null works", {
  expect_silent(correct_type(NULL, NULL))
  expect_silent(correct_type("T", NULL))
  expect_silent(correct_type(NULL, 1:12))
})

context("filter_row / correct_filters")

test_that("null works", {
  expect_silent(correct_filters(NULL, NULL))
  expect_silent(correct_filters("T", NULL))
  expect_silent(correct_filters(NULL, 1:12))
})

test_that("all inputs return expected type", {
  df <- data.frame(
    A = Sys.Date() - 1:26, B = as.character(Sys.Date() - 1:26),
    C = 1:26, D = as.character(1:26), E = letters,
    F = rep_len(letters[1:4], 26), stringsAsFactors = FALSE
  )
  expect_silent(res <- correct_filters(NA, df))
  expect_equal(res, c("D", "D", "R", "R", "T", "S"))
  expect_silent(res <- correct_filters("R", df))
  expect_equal(res, c("R", "R", "R", "R", "T", "S"))
  expect_silent(res <- correct_filters("D", df))
  expect_equal(res, c("D", "D", "R", "R", "T", "S"))
  expect_silent(res <- correct_filters("S", df))
  expect_equal(res, c("S", "S", "S", "S", "S", "S"))
})

test_that("inputs with NAs still return expected type", {
  df <- data.frame(
    A = Sys.Date() - 1:26, B = as.character(Sys.Date() - 1:26),
    C = 1:26, D = as.character(1:26), E = letters,
    F = rep_len(letters[1:4], 26), stringsAsFactors = FALSE
  )
  df[c(TRUE, FALSE),] <- NA
  expect_silent(res <- correct_filters(NA, df))
  expect_equal(res, c("D", "D", "R", "R", "T", "S"))
  expect_silent(res <- correct_filters("R", df))
  expect_equal(res, c("R", "R", "R", "R", "T", "S"))
  expect_silent(res <- correct_filters("D", df))
  expect_equal(res, c("D", "D", "R", "R", "T", "S"))
  expect_silent(res <- correct_filters("S", df))
  expect_equal(res, c("S", "S", "S", "S", "S", "S"))
})

context("filter_row / update_filters")

test_that("update_filters works with NULL inputs", {
  expect_null(update_filters(mtcars, NULL, NULL))
})

test_that("update_filters works with proper inputs", {
  df <- data.frame(
    A = letters, B = rep(c(TRUE, FALSE), 13), C = Sys.Date() - 0:25,
    D = 1:26, stringsAsFactors = FALSE
  )

  session <- create_test_session(
    id = "",
    list(
      "filter-A" = "g", "filter-B" = "TRUE", "filter-reset" = "test",
      "filter-C" = c(Sys.Date() - 20, Sys.Date() - 10), "filter-D" = c(12, 24)
    ),
    NULL
  )

  expect_null(update_filters(df, c("S", "S", "D", "R"), session))
  res <- session$lastInputMessages

  expect_equal(res[[1]]$id, "filter-A")
  expect_null(res[[1]]$message$value)
  expect_length(strsplit(res[[1]]$message$options, "</option>", fixed = TRUE)[[1]], 27) # letters + empty
  expect_equal(res[[2]]$id, "filter-B")
  expect_null(res[[2]]$message$value)
  expect_length(strsplit(res[[2]]$message$options, "</option>", fixed = TRUE)[[1]], 3) # true, false + empty
  expect_equal(res[[3]]$id, "filter-C")
  expect_equal(res[[3]]$message$min, as.character(Sys.Date() - 25))
  expect_equal(res[[3]]$message$max, as.character(Sys.Date()))
  expect_equal(res[[4]]$id, "filter-D")
  expect_equal(res[[4]]$message$min, "1")
  expect_equal(res[[4]]$message$max, "26")
})
