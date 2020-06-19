context("js / add_js")

test_that("add_js works with any input", {
  expect_silent(add_js("type", "function(params) {
    alert('Result is: ' + (parseInt(params[0]) + parseInt(params[1])));
  }"))
})

context("js / run_js")

test_that("run_js works with any input", {
  session <- create_test_session("", NULL, NULL)
  shiny::withReactiveDomain(session, {
    expect_silent(run_js(NULL, NULL))
    expect_silent(run_js(NULL, "params"))
    expect_silent(run_js("type", NULL))
    expect_silent(run_js("type", c("my", "params", "are")))
    expect_silent(run_js("type", list("my", "params", "are")))
  })
  m <- session$lastCustomMessages
  expect_null(m[[1]]$type)
  expect_equal(m[[2]]$message[[1]], "params")
  expect_null(m[[3]]$message[[1]])
  expect_length(m[[4]]$message[[1]], 3)
  expect_length(m[[5]]$message[[1]], 3)
})
