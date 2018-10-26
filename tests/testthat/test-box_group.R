context("box_group / create_dq_box_group")

test_that("bad inputs work", {
  expect_silent(create_dq_box_group(NULL))
})

test_that("all parameters work", {
  session <- list(input = list(A = "", B = ""))
  expect_silent(res <- create_dq_box_group(session, "A", "B"))
  expect_length(res, 2L)
  expect_s3_class(res[[1]], "Observer")
  expect_silent(res[[1]]$.func())
})

context("box_group / render_dq_box_group")

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
