context("dq_drawer / dq_drawer")

ids <- list(NULL, "string", 2, TRUE, c("my", "id", "is"), list("my", "id", "is"))

test_that("dq_drawer works with any input", {
  expect_silent(dq_drawer())
  expect_silent(dq_drawer(NULL))
  expect_silent(dq_drawer(NULL, NULL))
  expect_silent(dq_drawer(NULL, test = NULL))
  expect_silent(dq_drawer(test = "test"))
  expect_silent(dq_drawer(c = "content", size = "50px"))
  expect_silent(dq_drawer(c = "content", size = "80px", direction = "top"))
})

test_that("dq_drawer works with any combination of names", {
  expect_null(dq_drawer("id", list("content")))
  expect_true(grepl("dqdrawer-c", toString(dq_drawer("id", c = list("content")))))
  expect_true(grepl("dqdrawer-c", toString(dq_drawer("id", c = "content", "content2"))))
  expect_true(grepl("dqdrawer-c", toString(dq_drawer("id", "content", c = "content2"))))
  expect_true(grepl("dqdrawer-c", toString(dq_drawer("id", list("content" = "content2")))))
})

context("dq_drawer / update_dq_drawer")

test_that("update_dq_drawer sends message to client", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    expect_silent(update_dq_drawer(NULL, NULL))
    expect_silent(update_dq_drawer("id", TRUE))
    expect_silent(update_dq_drawer("id", FALSE))
  })
  m <- session$lastCustomMessages
  expect_equal(length(m), 2L)
  expect_equal(m[[1]]$type, "dqUpdateDrawer")
  expect_equal(m[[1]]$message$id, "id")
  expect_true(m[[1]]$message$value)
  expect_false(m[[2]]$message$value)
})

context("dq_drawer shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/dq_drawer"))
})
