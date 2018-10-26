context("dq_helptag / dq_helptag")

test_that("dq_helptag works with all inputs", {
  expect_silent(dq_helptag(NULL))
  expect_silent(dq_helptag("string"))
  expect_silent(dq_helptag(2))
  expect_silent(dq_helptag(TRUE))
  expect_silent(dq_helptag(NULL, NULL, NULL, NULL))
  expect_silent(dq_helptag(NULL, "hover", NA, "height: 200%;"))
  expect_silent(dq_helptag(NULL, NULL, "13px", "strange styles"))
  expect_silent(dq_helptag(NULL, "focus", "200vh", TRUE))
})
