context("js / add_js")

test_that("add_js works with any input", {
  if (requireNamespace("V8", quietly = TRUE)) {
    expect_error(add_js(NULL, NULL), "Error")
    expect_error(add_js(NULL, "function_text"), "ReferenceError")
    expect_error(add_js("type", NULL), "Error")
    expect_silent(add_js("type", "function(params) {
       alert('Result is: ' + (parseInt(params[0]) + parseInt(params[1])));
     }"))
  } else {
    expect_warning(add_js(NULL, NULL), "V8 package")
    expect_warning(add_js(NULL, "function_text"), "V8 package")
    expect_warning(add_js("type", NULL), "V8 package")
    expect_warning(add_js("type", "function(params) {
       alert('Result is: ' + (parseInt(params[0]) + parseInt(params[1])));
     }"), "V8 package")
  }
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

context("js / parse_code")

test_that("parse_code works with any input", {
  expect_warning(parse_code(NULL), "Nothing to parse")
  expect_error(parse_code("code"), "ReferenceError")
})
