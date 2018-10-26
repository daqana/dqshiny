context("video_box / video_box")

test_that("bad inputs work", {
  expect_silent(video_box(NULL, NULL))
})

test_that("all parameters work", {
  expect_true(grepl("id=\"id\"", video_box("id", NULL)))
  expect_true(grepl("src=\"src.*type=\"video/mp4", video_box("id", "src")))
  expect_true(grepl("type=\"test", video_box("id", "src", type = "test")))
  expect_true(grepl("title\">title", video_box("id", NULL, "title")))
})

context("video_box / video_tag")

test_that("bad inputs work", {
  expect_silent(video_tag(NULL))
})

test_that("all parameters work", {
  expect_true(grepl("id_wrapper", video_tag("id")))
  expect_true(grepl("currentTime = 25", video_tag("id", 25)))
  expect_true(grepl("&#39;strfg&#39;", video_tag("id", title = "strfg")))
})
