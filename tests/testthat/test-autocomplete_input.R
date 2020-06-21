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
  expect_true(grepl("eate=\"true", aI("id", NULL, NULL, create = TRUE)))
  expect_true(grepl("tains=\"true", aI("id", NULL, NULL, contains = TRUE)))
})

context("autocomplete_input / update_autocomplete_input")

test_that("function works like shiny functions", {
  expect_error(update_autocomplete_input(NULL, NULL), "Nicht|non")
})

test_that("all parameters work", {
  session <- create_test_session("", NULL, NULL)

  update_autocomplete_input(session, "id")
  expect_equal(session$lastInputMessages[[1]]$id, "id")
  update_autocomplete_input(session, "id", "label")
  expect_equal(session$lastInputMessages[[2]]$message$label, "label")
  update_autocomplete_input(session, "id", options = 1:500)
  expect_equal(session$lastInputMessages[[3]]$message$options, 1:500)
  update_autocomplete_input(session, "id", max_options = 500)
  expect_equal(session$lastInputMessages[[4]]$message$max, 500)
  update_autocomplete_input(session, "id", value = 5)
  expect_equal(session$lastInputMessages[[5]]$message$value, 5)
  update_autocomplete_input(session, "id", placeholder = "pl")
  expect_equal(session$lastInputMessages[[6]]$message$placeholder, "pl")
  update_autocomplete_input(session, "id", hide_values = TRUE)
  expect_equal(session$lastInputMessages[[7]]$message$hide, TRUE)
  update_autocomplete_input(session, "id", create = TRUE)
  expect_equal(session$lastInputMessages[[8]]$message$create, TRUE)
  update_autocomplete_input(session, "id", contains = TRUE)
  expect_equal(session$lastInputMessages[[9]]$message$contains, TRUE)
})

context("autocomplete_input / shinytest")

skip_on_cran()
skip_on_travis()

test_that("shinytest will be passed", {
  shinytest::expect_pass(shinytest::testApp("shinytest/autocomplete_input"))
})
