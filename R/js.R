#' Adds a custom js function
#'
#' @description Adds the given js function definition to the current shiny web
#' app.
#'
#' Before adding anything the code will be parsed with V8 if the package is
#' available on your machine. If package is available and parsing fails, an
#' error will be thrown.
#'
#' After successfully adding a function it can be used with run_js.
#'
#' @param type name of the function
#' @param function_text js function definition, this should be an anonymous
#' function accepting exactly one argument
#'
#' @export
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     add_js(type="addValues", "function(params) {
#'       alert('Result is: ' + (parseInt(params[0]) + parseInt(params[1])));
#'     }"),
#'     add_js(type="myName", "function(params) {
#'       alert('My name is ' + params.name);
#'     }"),
#'     actionButton("btn1", "Add Values"),
#'     actionButton("btn2", "What's my name?")
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$btn1, run_js(type = "addValues", 17, 25))
#'     observeEvent(input$btn2, run_js(type = "myName", name = "Paul"))
#'   }
#' )
#'
#' }
add_js <- function(type, function_text) {
  code <- paste0(
    "Shiny.addCustomMessageHandler('", type, "', ", function_text, ");"
  )
  parse_code(code)
  shiny::tags$head(shiny::tags$script(code))
}

#' @param ... arguments to pass to the function, those will be combined to a
#' list and end up as an array in js, unnamed parameters will be available via
#' params[0..], named parameters can also be used with params.name
#'
#' @export
#' @rdname add_js
run_js <- function(type, ...) {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$sendCustomMessage(type = type, message = list(...))
  }
}

#' @author richard.kunze
parse_code <- function(code) {
  if (!requireNamespace("V8", quietly = TRUE)) {
    warning("Package 'V8' not installed, so custom js code can't be checked!")
  } else {
    ct <- V8::new_context("Shiny", FALSE, FALSE)
    ct$eval("Shiny.addCustomMessageHandler=function(a,b){};")
    tryCatch({
      if (!is.null(code)) {
        ct$eval(code)
      } else {
        warning("Nothing to parse here!")
      }
    }, error = function(e) {
      stop(paste("Error found while parsing your js code:", e$message))
    })
  }
}
