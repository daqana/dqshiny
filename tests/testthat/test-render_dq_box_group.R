context("Test render_dq_box_group in box_group.R")

test_that("bad inputs work", {
  expect_silent(render_dq_box_group())
})

test_that("all parameters work", {
  expect_silent(res <- render_dq_box_group(
    dq_box(collapsible = TRUE),
    dq_box(id = "Id", title = "test"),
    dq_box(),
    open = 2
  ))
  expect_s3_class(res, "shiny.render.function")
  session <- shiny::getDefaultReactiveDomain()
  expect_true(grepl("Id_collapser", res(session)$html, fixed = TRUE))
})
