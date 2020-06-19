context("dqshiny / dq_NS")

test_that("dq_NS works with any input", {
  expect_silent(dq_NS(NULL))
  expect_silent(dq_NS(""))
  expect_silent(dq_NS(character()))
  expect_equal(dq_NS("", "abc"), "-abc")
  expect_equal(dq_NS("abc", "def"), "abc-def")
  expect_equal(dq_NS("abc")(), "abc")
  expect_equal(dq_NS(character())(), character())
  expect_equal(dq_NS("")(), "")
})
