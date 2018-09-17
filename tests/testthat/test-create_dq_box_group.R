context("Test create_dq_box_group in box_group.R")

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
