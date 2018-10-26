context("icon_state_button / icon_state_button")

test_that("bad inputs work", {
  expect_silent(icon_state_button(NULL, NULL))
})

test_that("all parameters work", {
  expect_true(grepl("id=\"id\"", icon_state_button("id", NULL)))
  hands <- paste0("hand-o-", c("up", "right", "down", "left"))
  expect_true(grepl("fa-hand-o-up", icon_state_button("id", hands)))
  expect_true(grepl("fa-hand-o-right", icon_state_button("id", hands, 2)))
  expect_true(grepl("fa-hand-o-left", icon_state_button("id", hands, hands[4])))
  expect_true(grepl("fa-hand-o-up", icon_state_button("id", hands, "test")))
  expect_true(grepl("style=\"col", icon_state_button("id", hands, style = "color:red;")))
})

context("icon_state_button / update_icon_state_button")

test_that("function works like shiny functions", {
  expect_error(update_icon_state_button(NULL, NULL), "Nicht|non")
})

test_that("all parameters work", {
  e <- "Nicht|non"
  expect_error(update_icon_state_button(NULL, "id"), e)
  expect_error(update_icon_state_button(NULL, "id", 1:5), e)
  expect_error(update_icon_state_button(NULL, "id", value = 5), e)
  expect_error(update_icon_state_button(NULL, "id", value = "test"), e)
  expect_error(update_icon_state_button(NULL, "id", "test", "test"), e)
})
