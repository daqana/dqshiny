context("css_class / add_class")

test_that("add_class works with any input", {
  expect_null(add_class(NULL, NULL))
  expect_null(add_class("string", "string"))
  expect_null(add_class(2, 2))
  expect_null(add_class(TRUE, FALSE))
  expect_null(add_class(c("my", "id", "is"), "string"))
  expect_null(add_class(list("my", "id", "is"), "string"))
})

context("css_class / remove_class")

test_that("remove_class works with any input", {
  expect_null(remove_class(NULL, "string"))
  expect_null(remove_class("string", 2))
  expect_null(remove_class(2, FALSE))
  expect_null(remove_class(TRUE, NULL))
  expect_null(remove_class(c("my", "id", "is"), "string"))
  expect_null(remove_class(list("my", "id", "is"), "string"))
})

context("css_class / toggle_class")

test_that("toggle_class works with any input", {
  expect_null(toggle_class(NULL, 2))
  expect_null(toggle_class("string", FALSE))
  expect_null(toggle_class(2, NULL))
  expect_null(toggle_class(TRUE, "string"))
  expect_null(toggle_class(c("my", "id", "is"), "string"))
  expect_null(toggle_class(list("my", "id", "is"), "string"))
})

test_that("toggle_class works with conditions", {
  expect_null(toggle_class("id", "class", NULL))
  expect_null(toggle_class("id", "class", TRUE))
  expect_null(toggle_class("id", "class", FALSE))

})

test_that("toggle_class works with different class inputs", {
  expect_null(toggle_class("id", NULL, TRUE))
  expect_null(toggle_class("id", "class", TRUE))
  expect_null(toggle_class("id", 2, TRUE))
  expect_null(toggle_class("id", FALSE, TRUE))
})

context("css_class / shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/css_class"))
})
