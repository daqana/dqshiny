context("display / hide")

ids <- list(NULL, "string", 2, TRUE, c("my", "id", "is"), list("my", "id", "is"))

test_that("hide works with any input", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    lapply(ids, function(id) expect_silent(hide(id)))
  })
  lapply(session$lastCustomMessages, function(x) {
    expect_equal(x$type, "toggleClass")
    expect_equal(x$message$className, "hidden")
    expect_equal(x$message$state, TRUE)
    expect_true(x$message$id %in% unlist(ids))
  })
})

context("display / show")

test_that("show works with any input", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    lapply(ids, function(id) expect_silent(show(id)))
  })
  lapply(session$lastCustomMessages, function(x) {
    expect_equal(x$type, "toggleClass")
    expect_equal(x$message$className, "hidden")
    expect_equal(x$message$state, FALSE)
    expect_true(x$message$id %in% unlist(ids))
  })
})

context("display / toggle")

test_that("toggle works with any input", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    lapply(ids, function(id) expect_silent(toggle(id)))
  })
  lapply(session$lastCustomMessages, function(x) {
    expect_equal(x$type, "toggleClass")
    expect_equal(x$message$className, "hidden")
    expect_null(x$message$state)
    expect_true(x$message$id %in% unlist(ids))
  })
})

test_that("toggle works with conditions", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    expect_silent(toggle("id", NULL))
    expect_silent(toggle("id", TRUE))
    expect_silent(toggle("id", FALSE))
  })
  m <- session$lastCustomMessages
  expect_null(m[[1]]$message$state)
  expect_false(m[[2]]$message$state)
  expect_true(m[[3]]$message$state)
})

context("display shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/display"))
})
