context("css_class / add_class")

ids <- list(NULL, "string", 2, TRUE, c("my", "id", "is"), list("my", "id", "is"))
classes <- list(2, NULL, "string", FALSE, "string", TRUE)

test_that("add_class works with any input", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    lapply(seq(ids), function(i) expect_silent(add_class(ids[[i]], classes[[i]])))
  })
  lapply(session$lastCustomMessages, function(x) {
    expect_equal(x$type, "toggleClass")
    expect_true(is.null(x$message$className) || x$message$className %in% classes)
    expect_true(x$message$state)
    expect_true(x$message$id %in% unlist(ids))
  })
})

context("css_class / remove_class")

test_that("remove_class works with any input", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    lapply(seq(ids), function(i) expect_silent(remove_class(ids[[i]], classes[[i]])))
  })
  lapply(session$lastCustomMessages, function(x) {
    expect_equal(x$type, "toggleClass")
    expect_true(is.null(x$message$className) || x$message$className %in% classes)
    expect_false(x$message$state)
    expect_true(x$message$id %in% unlist(ids))
  })
})

context("css_class / toggle_class")

test_that("toggle_class works with any input", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    lapply(seq(ids), function(i) expect_silent(toggle_class(ids[[i]], classes[[i]])))
  })
  lapply(session$lastCustomMessages, function(x) {
    expect_equal(x$type, "toggleClass")
    expect_true(is.null(x$message$className) || x$message$className %in% classes)
    expect_null(x$message$state)
    expect_true(x$message$id %in% unlist(ids))
  })
})

test_that("toggle_class works with conditions", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    expect_silent(toggle_class("id", "class", NULL))
    expect_silent(toggle_class("id", "class", TRUE))
    expect_silent(toggle_class("id", "class", FALSE))
  })
  m <- session$lastCustomMessages
  expect_null(m[[1]]$message$state)
  expect_true(m[[2]]$message$state)
  expect_false(m[[3]]$message$state)
})

test_that("toggle_class works with different class inputs", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    expect_silent(toggle_class("id", NULL, TRUE))
    expect_silent(toggle_class("id", "class", TRUE))
    expect_silent(toggle_class("id", 2, TRUE))
    expect_silent(toggle_class("id", FALSE, TRUE))
  })
  m <- session$lastCustomMessages
  expect_null(m[[1]]$message$className)
  expect_equal(m[[2]]$message$className, "class")
  expect_equal(m[[3]]$message$className, 2)
  expect_false(m[[4]]$message$className)
})

context("css_class / shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/css_class"))
})
