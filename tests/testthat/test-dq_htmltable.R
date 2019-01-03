context("dq_htmltable / dq_htmltable")

test_that("bad inputs work", {
  expect_equal(dq_htmltable(NULL, NULL, NULL), shiny::tags$table(class="dq-html-table", NULL, list(), init()))
})

test_that("alignments work", {
  el <- list(c("a", "b"), c("c", "d"))
  a <- list(c("left", "left"), c("left", "left"))
  expect_equal(dq_htmltable(el), dq_htmltable(el, align = a))
  expect_equal(dq_htmltable(el, align = "l"), dq_htmltable(el, align = a))
  expect_equal(dq_htmltable(el, align = c("left", "l")), dq_htmltable(el, align = a))
  expect_equal(dq_htmltable(el, align = list("L", "lEft")), dq_htmltable(el, align = a))
})

test_that("borders without header work", {
  el <- list(c("a", "b"), c("c", "d"))
  expect_silent(dq_htmltable(el))
  expect_silent(res <- dq_htmltable(el, borders = "inner"))
  expect_true(grepl("right bottom$", res$children[[2]][[1]]$children[[1]][[1]]$attribs$class))
  expect_silent(res <- dq_htmltable(el, borders = "outer"))
  expect_true(grepl("left top$", res$children[[2]][[1]]$children[[1]][[1]]$attribs$class))
  expect_silent(res <- dq_htmltable(el, borders = "tex"))
  expect_true(grepl("top$", res$children[[2]][[1]]$children[[1]][[1]]$attribs$class))
  expect_silent(res <- dq_htmltable(el, borders = "all"))
  expect_true(grepl("right left bottom top$", res$children[[2]][[1]]$children[[1]][[1]]$attribs$class))
  expect_silent(res <- dq_htmltable(el, borders = list(
    c("left right", ""), c("", "")
  )))
  expect_true(grepl("left right$", res$children[[2]][[1]]$children[[1]][[1]]$attribs$class))
  expect_silent(dq_htmltable(el, borders = NULL))
})

test_that("borders with header work", {
  el <- data.frame(A = c("a", "b"), B = c("c", "d"))
  expect_silent(dq_htmltable(el))
  expect_silent(res <- dq_htmltable(el, borders = "inner"))
  expect_true(grepl("right bottom$", res$children[[1]]$children[[1]][[1]]$attribs$class))
  expect_silent(res <- dq_htmltable(el, borders = "outer"))
  expect_true(grepl("left top bottom$", res$children[[1]]$children[[1]][[1]]$attribs$class))
  expect_silent(res <- dq_htmltable(el, borders = "tex"))
  expect_true(grepl("top bottom$", res$children[[1]]$children[[1]][[1]]$attribs$class))
  expect_silent(res <- dq_htmltable(el, borders = "all"))
  expect_true(grepl("left right top bottom$", res$children[[1]]$children[[1]][[1]]$attribs$class))
  expect_silent(res <- dq_htmltable(el, borders = list(
    c("left right", ""), c("", ""), c("", "")
  )))
  expect_true(grepl("left right$", res$children[[1]]$children[[1]][[1]]$attribs$class))
  expect_silent(dq_htmltable(el, borders = NULL))
})
