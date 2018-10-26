context("dq_infobox / dq_infobox")

test_that("dq_infobox works with any input", {
  expect_silent(dq_infobox(NULL))
  expect_silent(dq_infobox(NULL, 12))
  expect_silent(dq_infobox(NULL, NA, "message"))
  expect_silent(dq_infobox(NULL, Inf, TRUE, "icon"))
  expect_silent(dq_infobox(NULL, -12, NULL, NA, "#fff"))
  expect_silent(dq_infobox(NULL, href = "https://www.google.com"))
  expect_silent(dq_infobox(NULL, fill = NULL))
})
