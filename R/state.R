#' Change the state of a shiny interface element
#'
#' @description Those functions can be used to change an elements disabled
#' status.
#'
#' @note If you have trouble with these functions, please make sure that you
#' either - use any dqshiny element in your UI - load the package with
#' \code{\link{library}} - use \code{\link{init}} at the beginning of your UI!
#'
#' @param ids Character vector, id(s) of the element to enable/disable.
#'
#' @export
#' @family js handler
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     actionButton("btn_enable", "Enable"),
#'     actionButton("btn_disable", "Disable"),
#'     actionButton("btn_toggle", "Toggle State"),
#'     actionButton("btn_toggle_cond", "Toggle State with Condition"),
#'     actionButton("btn_toggle_all", "Toggle All States"),
#'     checkboxInput("condition", "Disabled"),
#'     dq_space(), # this is needed to make everything work
#'     actionButton("example1", "EXAMPLE1"),
#'     actionButton("example2", "EXAMPLE2"),
#'     actionButton("example3", "EXAMPLE3")
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$btn_enable, enable("example1"))
#'     observeEvent(input$btn_disable, disable("example1"))
#'     observeEvent(input$btn_toggle, toggle_state("example1"))
#'     observeEvent(input$btn_toggle_cond,
#'       toggle_state("example1", input$condition)
#'     )
#'     observeEvent(input$btn_toggle_all,
#'       toggle_state(c("example1", "example2", "example3"))
#'     )
#'   }
#' )
#'
#' }
enable <- function(ids) {
  send_message(type = "toggleState", ids = ids, state = FALSE)
}

#' @export
#' @rdname enable
disable <- function(ids) {
  send_message(type = "toggleState", ids = ids, state = TRUE)
}

#' @param condition Condition to be used for toggling the state (TRUE =
#' disabled).
#' @export
#' @rdname enable
toggle_state <- function(ids, condition = NULL) {
  send_message(type = "toggleState", ids = ids, state = condition)
}
