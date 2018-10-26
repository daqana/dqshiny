context("dq_busy / dq_busy")

test_that("dq_busy works with all inputs", {
  expect_is(dq_busy(), "shiny.tag")
  expect_is(dq_busy(NULL), "shiny.tag")
  expect_is(dq_busy(NULL, NULL), "shiny.tag")
  expect_is(dq_busy(NULL, NULL, NULL), "shiny.tag")
  expect_is(dq_busy("iconPath", "500", "noFadeIn"), "shiny.tag")
  expect_is(dq_busy(TRUE, FALSE, TRUE), "shiny.tag")
  expect_is(dq_busy(NULL, 1500, "fadeInTop"), "shiny.tag")
})
