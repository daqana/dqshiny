context("time_input / time_input")

test_that("bad inputs work", {
  expect_silent(time_input("id", NULL))
})

test_that("all parameters work", {
  tI <- time_input
  expect_true(grepl("id=\"id\"", tI("id", NULL)))
  expect_true(grepl("lab</lab", tI("id", "lab")))
  expect_true(grepl("width: 300px", tI("id", NULL, width = 300)))
  expect_true(grepl("holder=\"pl", tI("id", NULL, placeholder = "pl")))
  expect_true(grepl("lue=\"12:34", tI("id", NULL, value = "12:34")))
  expect_true(grepl("type=\"text", tI("id", NULL, use_material_picker = TRUE)))
})

context("time_input / update_time_input")

test_that("function works like shiny functions", {
  expect_error(update_time_input(NULL, NULL), "Nicht|non")
})

test_that("all parameters work", {
  e <- "Nicht|non"
  expect_error(update_time_input(NULL, "id"), e)
  expect_error(update_time_input(NULL, "id", "label"), e)
  expect_error(update_time_input(NULL, "id", value = 5), e)
  expect_error(update_time_input(NULL, "id", placeholder = "pl"), e)
  expect_error(update_time_input(NULL, "id", label = "lab"), e)
})

context("time_input / shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/time_picker"))
})
