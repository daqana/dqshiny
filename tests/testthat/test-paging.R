context("paging / update_page")

session <- create_test_session("", NULL, NULL)
test_that("update_page works with NULL inputs", {
  expect_null(update_page(NULL, NULL, NULL, NULL))
  expect_null(update_page(mtcars, NULL, NULL, NULL))
  expect_null(update_page(NULL, 1, 15, session))
  expect_null(update_page(mtcars, 1, 15, NULL))
})

test_that("update_page works with proper inputs", {
  expect_equal(update_page(mtcars, NULL, NULL, session), mtcars[1:25, ])
  expect_equal(update_page(mtcars, 2, NULL, session), mtcars[26:nrow(mtcars), ])
  expect_equal(update_page(mtcars, 2, 9, session), mtcars[10:18, ])
})

test_that("update_page works with strange inputs", {
  expect_equal(update_page(mtcars, 1, 200, session), mtcars)
  expect_equal(update_page(mtcars, 0, 200, session), mtcars)
  expect_equal(update_page(mtcars, -2, 9, session), mtcars[1:9, ])
  expect_equal(update_page(mtcars, NA, NA, session), mtcars[1:25, ])
  expect_equal(update_page(mtcars, NA, 9, session), mtcars[1:9, ])
  expect_equal(update_page(mtcars, 33, 25, session), mtcars[26:nrow(mtcars), ])
  expect_equal(update_page(mtcars, Inf, 1, session), mtcars[nrow(mtcars), ])
  expect_equal(update_page(mtcars, 1, Inf, session), mtcars)
  expect_equal(update_page(mtcars, Inf, Inf, session), mtcars)
})

context("paging / paging_row")

test_that("update_page works with NULL inputs", {
  expect_true(grepl("paging-row", paging_row(paste0, NULL, NULL)))
  expect_true(grepl("pageNum", paging_row(paste0, NULL, NULL)))
})

test_that("paging_row works with proper inputs", {
  expect_true(grepl("value=\"9\"", paging_row(paste0, 2, 9)))
})
