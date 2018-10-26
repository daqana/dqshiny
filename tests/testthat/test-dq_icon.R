context("dq_icon / dq_icon")

test_that("bad inputs work", {
  expect_silent(dq_icon(NULL))
  expect_silent(dq_icon(NULL, color = "test"))
  expect_silent(dq_icon(NULL, bg_color = "test"))
  expect_silent(dq_icon(NULL, size = "test"))
  expect_warning(dq_icon("", lib = "test"), "Unknown")
})

test_that("all parameters work", {
  expect_true(grepl("fa-phone", dq_icon("phone")))
  expect_true(htmltools::htmlDependencies(dq_icon(NULL))[[1]]$name == "font-awesome")
  expect_true(grepl("color:red;", dq_icon("", color = "red")))
  expect_true(grepl("ground-color:red;", dq_icon("", bg_color = "red")))
  expect_true(grepl("fa-4x", dq_icon("", size = "4x")))
  expect_true(grepl("ground:red", dq_icon("", style = "background:red")))
  expect_true(grepl("glyphicon-cog", dq_icon("cog", lib = "glyphicon")))
  expect_true(grepl("src=\"logo_daqana", dq_icon("logo_daqana.svg")))
})
