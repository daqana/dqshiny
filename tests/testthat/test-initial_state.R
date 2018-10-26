context("initial_state / hidden")

test_that("hidden works with any input", {
  expect_silent(hidden(NULL))
  expect_silent(hidden(shiny::fluidRow()))
  expect_silent(hidden(shiny::fluidRow(shiny::column(12, shiny::actionButton("btn", NULL)))))
  expect_silent(hidden(shiny::fluidRow(shiny::column(6), shiny::column(6))))
  expect_silent(hidden(shiny::tagList(shiny::fluidRow(), shiny::fluidRow())))
  expect_silent(hidden(list(shiny::fluidRow(), shiny::fluidRow())))
  expect_silent(hidden(shiny::fluidRow(), shiny::fluidRow()))
})

context("initial_state / disabled")

test_that("disabled works with any input", {
  expect_silent(disabled(NULL))
  expect_silent(disabled(shiny::actionButton(NULL, NULL)))
  expect_silent(disabled(shiny::textInput(NULL, NULL)))
  expect_silent(disabled(shiny::textAreaInput(NULL, NULL)))
  expect_silent(disabled(shiny::numericInput(NULL, NULL, 0)))
  expect_silent(disabled(shiny::dateInput(NULL, NULL)))
  expect_silent(disabled(shiny::sliderInput(NULL, NULL, 0, 30, 15)))
  expect_silent(disabled(shiny::dateRangeInput(NULL, NULL)))
  expect_silent(disabled(list(shiny::dateInput(NULL, NULL),
                                        shiny::textAreaInput(NULL, NULL))))
  expect_silent(disabled(shiny::fluidRow(shiny::numericInput(NULL, NULL, 0),
                                                   shiny::textInput(NULL, NULL))))
  expect_silent(disabled(shiny::actionButton(NULL, NULL),
                                   shiny::sliderInput(NULL, NULL, 0, 30, 15)))
})
