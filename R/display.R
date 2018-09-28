#' Change the state of a shiny interface element
#'
#' @description Those functions can be used to change an elements visibility
#' status.
#'
#' @note If you have trouble with these functions, please make sure that you
#' either - use any dqshiny element in your UI - load the package with
#' \code{\link{library}} - use \code{\link{init}} at the beginning of your UI!
#'
#' @param ids Character vector, id(s) of the element to hide/show.
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
#'     init(),
#'     actionButton("hide", "Hide"),
#'     actionButton("show", "Show"),
#'     actionButton("toggle", "Toggle"),
#'     actionButton("toggle_cond", "Toggle Visibility with Condition"),
#'     actionButton("toggle_all", "Toggle All Visibilities"),
#'     checkboxInput("condition", "Visible"),
#'     actionButton("example1", "EXAMPLE1"),
#'     actionButton("example2", "EXAMPLE2"),
#'     actionButton("example3", "EXAMPLE3")
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$hide, hide("example1"))
#'     observeEvent(input$show, show("example1"))
#'     observeEvent(input$toggle, toggle("example1"))
#'     observeEvent(input$toggle_cond,
#'       toggle("example1", input$condition)
#'     )
#'     observeEvent(input$toggle_all,
#'       toggle(c("example1", "example2", "example3"))
#'     )
#'   }
#' )
#'
#' }
hide <- function(ids) {
  add_class(ids, "hidden")
}

#' @export
#' @rdname hide
show <- function(ids) {
  remove_class(ids, "hidden")
}

#' @param condition Condition to be used for toggling the visibility (TRUE =
#' visible).
#' @export
#' @rdname hide
toggle <- function(ids, condition = NULL) {
  if (!is.null(condition)) {
    condition <- !condition
  }
  toggle_class(ids, "hidden", condition)
}
