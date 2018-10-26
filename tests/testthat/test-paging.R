context("paging / update_page")

session <- readRDS(file.path("data", "shinySession.RData"))
test_that("update_page works with NULL inputs", {
  expect_null(update_page(NULL, NULL, NULL, NULL, NULL))
  expect_null(update_page(mtcars, NULL, NULL, NULL, NULL))
  expect_null(update_page(NULL, "test", 1, 15, session))
  expect_null(update_page(mtcars, "test", 1, 15, NULL))
})

test_that("update_page works with proper inputs", {
  expect_equal(update_page(mtcars, NULL, NULL, NULL, session), mtcars[1:25, ])
  expect_equal(update_page(mtcars, NULL, 2, NULL, session), mtcars[26:nrow(mtcars), ])
  expect_equal(update_page(mtcars, NULL, 2, 9, session), mtcars[10:18, ])
})

test_that("update_page works with strange inputs", {
  expect_equal(update_page(mtcars, NULL, 1, 200, session), mtcars)
  expect_equal(update_page(mtcars, NULL, 0, 200, session), mtcars)
  expect_equal(update_page(mtcars, NULL, -2, 9, session), mtcars[1:9, ])
  expect_equal(update_page(mtcars, NULL, NA, NA, session), mtcars[1:25, ])
  expect_equal(update_page(mtcars, NULL, NA, 9, session), mtcars[1:9, ])
  expect_equal(update_page(mtcars, NULL, 33, 25, session), mtcars[26:nrow(mtcars), ])
  expect_equal(update_page(mtcars, NULL, Inf, 1, session), mtcars[nrow(mtcars), ])
  expect_equal(update_page(mtcars, NULL, 1, Inf, session), mtcars)
  expect_equal(update_page(mtcars, NULL, Inf, Inf, session), mtcars)
})

context("paging / paging_row")

session <- readRDS(file.path("data", "shinySession.RData"))
test_that("update_page works with NULL inputs", {
  expect_true(grepl("paging-row", paging_row(NULL, NULL, NULL)))
  expect_true(grepl("num_test_page", paging_row("test", NULL, NULL)))
})

test_that("paging_row works with proper inputs", {
  expect_true(grepl("value=\"9\"", paging_row("test", 2, 9)))
})
