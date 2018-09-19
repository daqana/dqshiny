#' Change the state of a shiny interface element
#'
#' @description Those functions can be used to change an elements visibility
#' status.
#'
#' @note All functions from this family won't work, if there is not at least
#' one dqshiny element in the UI. If there is absolutely no need to use any of
#' those elements, please add \code{\link{init}} to your UI.
#'
#' @param ids Character vector, id(s) of the element to hide/show.
#'
#' @export
#' @family js handler
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     actionButton("hide", "Hide"),
#'     actionButton("show", "Show"),
#'     actionButton("toggle", "Toggle"),
#'     actionButton("toggle_cond", "Toggle Visibility with Condition"),
#'     actionButton("toggle_all", "Toggle All Visibilities"),
#'     checkboxInput("condition", "Visible"),
#'     dq_space(), # this is needed to make everything work
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
#' )}
hide <- function(ids) {
  add_class(ids, "hidden")
}

#' @export
#' @rdname hide
show <- function(ids) {
  remove_class(ids, "hidden")
}

#' @param condition Condition to use for toggling the visibility.
#' @export
#' @rdname hide
toggle <- function(ids, condition = NULL) {
  if (!is.null(condition)) {
    condition <- !condition
  }
  toggle_class(ids, "hidden", condition)
}
