#' Change the state of a shiny interface element
#'
#' @description Those functions can be used to change an elements classes.
#'
#' @note All functions from this family won't work, if there is not at least
#' one dqshiny element in the UI. If there is absolutely no need to use any of
#' those elements, please add \code{\link{init}} to your UI.
#'
#' @param ids Character vector, id(s) of the element(s) to change.
#' @param class_name Name of the class to add/remove/toggle.
#'
#' @export
#' @family js handler
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     tags$head(tags$style(".orange{background:#ff8f00;}")),
#'     actionButton("btn_add", "add_class"),
#'     actionButton("btn_remove", "remove_class"),
#'     actionButton("btn_toggle", "toggle_class"),
#'     actionButton("btn_toggle_cond", "Toggle Class with Condition"),
#'     checkboxInput("condition", "orange"),
#'     fluidRow(id = "example_bg",
#'       dq_space(), # this is needed to make everything work
#'       actionButton("example", "EXAMPLE"),
#'       dq_space() # this is just for the alignment ;)
#'    )
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$btn_add, add_class("example_bg", "orange"))
#'     observeEvent(input$btn_remove, remove_class("example_bg", "orange"))
#'     observeEvent(input$btn_toggle, toggle_class("example_bg", "orange"))
#'     observeEvent(
#'       input$btn_toggle_cond,
#'       toggle_class("example_bg", "orange", input$condition)
#'     )
#'   }
#' )}
add_class <- function(ids, class_name) {
  send_message(
    type = "toggleClass",
    id = unname(ids),
    class_name = class_name,
    state = TRUE
  )
}

#' @export
#' @rdname add_class
remove_class <- function(ids, class_name) {
  send_message(
    type = "toggleClass",
    id = unname(ids),
    class_name = class_name,
    state = FALSE
  )
}

#' @param condition Condition to use for toggling the class.
#' @export
#' @rdname add_class
toggle_class <- function(ids, class_name, condition = NULL) {
  send_message(
    type = "toggleClass",
    id = unname(ids),
    class_name = class_name,
    state = condition
  )
}
