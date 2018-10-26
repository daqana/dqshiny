context("js / add_js")

test_that("add_js works with any input", {
  if (requireNamespace("V8", quietly = TRUE)) {
    expect_error(add_js(NULL, NULL), "SyntaxError")
    expect_error(add_js(NULL, "function_text"), "ReferenceError")
    expect_error(add_js("type", NULL), "SyntaxError")
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
  expect_null(run_js(NULL, NULL))
  expect_null(run_js(NULL, "params"))
  expect_null(run_js("type", NULL))
  expect_null(run_js("type", c("my", "params", "are")))
  expect_null(run_js("type", list("my", "params", "are")))
})

context("js / parse_code")

test_that("parse_code works with any input", {
  expect_warning(parse_code(NULL), "Nothing to parse")
  expect_error(parse_code("code"), "ReferenceError")
})
