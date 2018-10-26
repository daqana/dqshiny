context("hot_helpers / dq_as_selectize_options")

test_that("null works", {
  expect_equal(dq_as_selectize_options(NULL), list(options = list()))
  expect_equal(dq_as_selectize_options(NULL, NULL), list(options = list(), NULL))
  expect_equal(dq_as_selectize_options(NULL, "a" = NULL), list(options = list(), a = NULL))
})

test_that("valid inputs work", {
  expect_equal(dq_as_selectize_options(NA), list(options = list(list(value = NA, text = NA_character_))))
  expect_equal(dq_as_selectize_options(NA, random = "more random"),
               list(options = list(list(value = NA, text = NA_character_)), random = "more random"))
  res <- list(options = list(list(value = 1, text = "1"), list(value = 2, text = "2"),
                             list(value = 3, text = "3"), list(value = 4, text = "4")))
  expect_equal(dq_as_selectize_options(1:4), res)
})

test_that("named inputs work", {
  inp <- 1:4
  names(inp) <- c("Hello", "my", "funny", "World!")
  res <- list(options = list(list(value = 1, text = "Hello"), list(value = 2, text = "my"),
                             list(value = 3, text = "funny"), list(value = 4, text = "World!")))
  expect_equal(dq_as_selectize_options(inp), res)
})

context("hot_helpers / dq_add_selectize_options")

hot <- rhandsontable::rhandsontable(mtcars)

test_that("null works and always returns hot element", {
  expect_equal(dq_add_selectize_options(NULL, NULL, NULL, NULL), NULL)
  expect_equal(dq_add_selectize_options(NULL, 1, 2, NULL), NULL)
  expect_equal(dq_add_selectize_options(NULL, 1, 2, 1:5), NULL)
})

test_that("bad inputs work and always returns hot element", {
  expect_equal(
    dq_add_selectize_options(
      list(x = list(cells = list())), 1, 2, list()
    ), list(x = list(cells = list()))
  )
  expect_equal(
    dq_add_selectize_options("random", NULL, NULL, NULL), "random"
  )
  expect_equal(
    dq_add_selectize_options(TRUE, NULL, 2, list(of = "options")),
    TRUE
  )
  expect_equal(
    dq_add_selectize_options(hot, NULL, 2, NULL)$dependencies[[1]]$name,
    "dqSelectize"
  )
})

test_that("proper inputs work", {
  expect_silent(res <- dq_add_selectize_options(hot, 1, 2, 1:5))
  expect_equal(names(res$x$cell[[1]]), c("row", "col", "type", "editor", "selectizeOptions"))
  expect_silent(
    res <- dq_add_selectize_options(hot, 1:7, "disp", list(1:5, numeric(), 1:3, 1, numeric(), 1:5, 2:8))
  )
  expect_length(res$x$cell, 5)
  expect_silent(res <- dq_add_selectize_options(hot, NULL, 3, list(1:2, 1:3)))
  expect_length(res$x$cell, nrow(mtcars))
})

context("hot_helpers / dq_hot_cell")

test_that("null works and always returns hot element", {
  expect_equal(dq_hot_cell(NULL, NULL, NULL), NULL)
  expect_equal(dq_hot_cell(NULL, 1, 2), NULL)
  expect_equal(dq_hot_cell(hot, NULL, NULL), hot)
})

test_that("bad inputs work and always returns hot element", {
  expect_equal(dq_hot_cell(list(x = list(cells = list())), 1, 2),
               list(x = list(cells = list())))
  expect_equal(dq_hot_cell("random", NULL, NULL), "random")
  expect_equal(dq_hot_cell(T, NULL, 2), TRUE)
})

test_that("scalar and vectorized inputs work", {
  expect_silent(res <- dq_hot_cell(hot, 1, 2, readOnly = TRUE))
  expect_equal(res$x$cell, list(list(row = 0, col = 1, readOnly = TRUE)))
  expect_silent(res <- dq_hot_cell(hot, 1:5, 2:3, customKey = "customValue"))
  expect_length(res$x$cell, 10)
})
