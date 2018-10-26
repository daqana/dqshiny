context("autocomplete_input / autocomplete_input")

test_that("bad inputs work", {
  expect_silent(autocomplete_input("id", NULL, NULL))
})

test_that("all parameters work", {
  aI <- autocomplete_input
  expect_true(grepl("id=\"id\"", aI("id", NULL, NULL)))
  expect_true(grepl("lab</lab", aI("id", "lab", NULL)))
  expect_true(grepl("\\[1,2,3", aI("id", NULL, 1:100)))
  expect_true(grepl("value=\"5", aI("id", NULL, NULL, 5)))
  expect_true(grepl("width: 300px", aI("id", NULL, NULL, width = 300)))
  expect_true(grepl("holder=\"pl", aI("id", NULL, NULL, placeholder = "pl")))
  expect_true(grepl("max=\"300", aI("id", NULL, NULL, max_options = 300)))
  expect_true(grepl("hide=\"true", aI("id", NULL, NULL, hide_values = TRUE)))
})

context("autocomplete_input / update_autocomplete_input")

test_that("function works like shiny functions", {
  expect_error(update_autocomplete_input(NULL, NULL), "Nicht|non")
})

test_that("all parameters work", {
  e <- "Nicht|non"
  expect_error(update_autocomplete_input(NULL, "id"), e)
  expect_error(update_autocomplete_input(NULL, "id", "label"), e)
  expect_error(update_autocomplete_input(NULL, "id", options = 1:500), e)
  expect_error(update_autocomplete_input(NULL, "id", max_options = 500), e)
  expect_error(update_autocomplete_input(NULL, "id", value = 5), e)
  expect_error(update_autocomplete_input(NULL, "id", placeholder = "pl"), e)
  expect_error(update_autocomplete_input(NULL, "id", hide_values = TRUE), e)
})

context("autocomplete_input / shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/autocomplete_input"))
})
