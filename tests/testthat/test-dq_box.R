context("dq_box / dq_box")

test_that("dq_box works with bad inputs", {
  expect_silent(dq_box())
})

test_that("dq_box works with all parameters", {
  expect_true(grepl("no-header", dq_box(), fixed = TRUE))
  expect_true(grepl("id=\"testId\"", dq_box(id = "testId"), fixed = TRUE))
  expect_true(grepl("dq-box-title.*TestTitle", dq_box(title = "TestTitle")))
  expect_true(grepl("color:red", dq_box(title = "", color = "red")))
  expect_true(grepl("background:red", dq_box(bg_color = "red")))
  expect_true(grepl("col-sm-offset-3", dq_box(offset = 3)))
  expect_true(grepl("col-sm-3", dq_box(width = 3)))
  expect_true(grepl("height:500px", dq_box(height = 500)))
  expect_true(grepl("not-filled.*-width:0 2px 2px", dq_box(fill = FALSE)))
  expect_true(grepl("collapsible.* in\"", dq_box(collapsible = TRUE)))
  expect_true(grepl("collapse \"", dq_box(collapsed = TRUE)))
  expect_true(grepl("collapse.*click=\"Shiny", dq_box(open_callback = TRUE)))
})

context("dq_box / update_dq_box")

test_that("works with all inputs", {
  expect_silent(update_dq_box(NULL, NULL))
  expect_silent(update_dq_box("string", TRUE))
  expect_silent(update_dq_box(2, FALSE))
  expect_silent(update_dq_box(TRUE, TRUE, TRUE))
})
