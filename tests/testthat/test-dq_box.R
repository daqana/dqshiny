context("Test dq_box in dq_box.R")

test_that("dq_box works with all inputs", {
  expect_silent(dq_box(NULL))
  expect_error(dq_box(color = NULL, bg_color = NULL, fill = NULL, offset = NULL, width = NULL, collapsible = NULL, collapsed = NULL), "width")
  expect_silent(dq_box("This is the content", height = "200px", title = "This is the title", color = "#877634", collapsible = TRUE))
  expect_silent(dq_box("This is the content", height = 495, bg_color = "red", collapsible = TRUE, collapsed = TRUE))
})
