context("dq_htmltable / dq_htmltable")

test_that("bad inputs work", {
  expect_equal(dq_htmltable(NULL, NULL, NULL), shiny::tags$table(list(), style = "width:100%;", init()))
})

test_that("alignments work", {
  el <- list(c("a", "b"), c("c", "d"))
  a <- list(c("left", "left"), c("left", "left"))
  expect_equal(dq_htmltable(el), dq_htmltable(el, align = a))
  expect_equal(dq_htmltable(el, align = "l"), dq_htmltable(el, align = a))
  expect_equal(dq_htmltable(el, align = "left"), dq_htmltable(el, align = a))
  expect_equal(dq_htmltable(el, align = c("left", "l")), dq_htmltable(el, align = a))
  expect_equal(dq_htmltable(el, align = list("l", "left")), dq_htmltable(el, align = a))
  expect_equal(dq_htmltable(el, align = list(c("l", "left"), "left")), dq_htmltable(el, align = a))
  expect_equal(dq_htmltable(el, align = list(c("l", "l"), c("l", "l"))), dq_htmltable(el, align = a))
})
