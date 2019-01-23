context("state / enable")

ids <- list(NULL, "string", 2, TRUE, c("my", "id", "is"), list("my", "id", "is"))

test_that("enable works with any input", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    lapply(ids, function(id) expect_silent(enable(id)))
  })
  lapply(session$lastCustomMessages, function(x) {
    expect_equal(x$type, "toggleState")
    expect_equal(x$message$state, FALSE)
    expect_true(x$message$id %in% unlist(ids))
  })
})

context("state / disable")

test_that("disable works with any input", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    lapply(ids, function(id) expect_silent(disable(id)))
  })
  lapply(session$lastCustomMessages, function(x) {
    expect_equal(x$type, "toggleState")
    expect_equal(x$message$state, TRUE)
    expect_true(x$message$id %in% unlist(ids))
  })
})

context("state / toggle_state")

test_that("toggle_state works with any input", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    lapply(ids, function(id) expect_silent(toggle_state(id)))
  })
  lapply(session$lastCustomMessages, function(x) {
    expect_equal(x$type, "toggleState")
    expect_null(x$message$state)
    expect_true(x$message$id %in% unlist(ids))
  })
})

test_that("toggle_state works with conditions", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    expect_silent(toggle_state("id", NULL))
    expect_silent(toggle_state("id", TRUE))
    expect_silent(toggle_state("id", FALSE))
  })
  m <- session$lastCustomMessages
  expect_null(m[[1]]$message$state)
  expect_true(m[[2]]$message$state)
  expect_false(m[[3]]$message$state)
})

context("state / shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/state"))
})
