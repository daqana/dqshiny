context("dq_accordion / dq_accordion")

test_that("dq_accordion works with NULL inputs", {
  expect_silent(dq_accordion(NULL, NULL, NULL))
  expect_silent(dq_accordion("id", NULL, NULL))
  expect_silent(dq_accordion(NULL, c("title1", "title2"), NULL))
  expect_silent(dq_accordion(NULL, NULL,
                             list(shiny::tags$p("content1"), "content2")))
})

test_that("dq_accordion works with bad inputs", {
  expect_warning(dq_accordion(
    "id", c("title1", "title2"),
    list(shiny::tags$p("content1"), "content2", "content3")
  ), "Unmatching lengths")
  expect_warning(dq_accordion(
    "id", c("title1", "title2", "title3"),
    list(shiny::tags$p("content1"), "content2")
  ), "Unmatching lengths")
  expect_equal(dq_accordion(
    "id", c("title1", "title1", "title1"),
    list("content1", "content2", "content3")
  ), suppressWarnings(
    dq_accordion("id", "title1", list("content1", "content2", "content3"))
  ))
  expect_warning(dq_accordion(
    "id", "title1", "content1", icons = list(bad = "bad", good = "good")
  ), "Unnamed icon vector found")
  expect_warning(dq_accordion(
    "id", "title1", "content1", icons = list("bad", "worse", "worst")
  ), "Incorrect")
  expect_silent(dq_accordion(
    "id", "title1", "content1", icons = c(bad = "bad")
  ))
})

test_that("dq_accordion works with all inputs", {
  expect_is(dq_accordion(
    "id", c("title1", "title2"), list("content1", "content2"),
    bg_color = NULL, icons = NULL, sortable = TRUE
  ), "shiny.tag")
  expect_is(dq_accordion(
    "id", c("title1", "title2"), list("content1", "content2"),
    bg_color = "#666", options = list(animation = 900, collapsible = TRUE)
  ), "shiny.tag")
  expect_is(dq_accordion(
    "id", c("title1", "title2"), list("content1", "content2"),
    icons = c(rotate = "hand-o-right")
  ), "shiny.tag")
  expect_is(dq_accordion(
    "id", c("title1", "title2"), list("content1", "content2"),
    icons = c(open = "hand-o-down", closed = "hand-o-right")
  ), "shiny.tag")
})

context("dq_accordion / accordion_script")

test_that("accordion_script works with all inputs", {
  testthat::skip_if_not_installed("V8")

  a <- V8::new_context()
  a$eval("$ = function(){}")

  expect_silent(a$eval(accordion_script("id", NULL, FALSE)))
  expect_silent(a$eval(accordion_script(
    "id", list(animate = 500, collapsible = T), FALSE)
  ))
  expect_silent(a$eval(accordion_script(
    "id", list(icons = list(
      header = "fa-hand-o-right", activeHeader = "fa-hand-o-down")
    ), TRUE)
  ))
})
