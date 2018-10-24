#' Creates a help symbol with the given title as popover
#'
#' @description Creates a help symbol with the given title as a popover.
#' Trigger to show the popover can either be a mouse hover or a mouse click
#' to show it until the user clicks again. Info tags can be added to shiny
#' input labels via tagLists.
#'
#' @param title Title to show in the popover.
#' @param trigger Optional, can either be 'hover' to show the popover while the
#'   mouse is over the icon or 'focus' to show it while the tag is in focus.
#' @param width Optional, width of the popover, can be numeric or character
#'   including any valid CSS unit.
#' @param style Optional character, additional style attributes for the tag.
#'
#' @return shiny tag holding the help icon
#' @export
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     fluidRow(
#'       column(3,
#'         dq_helptag(
#'         "This info is visible after an click!<br>
#'         Line breaks are also possible btw...",
#'                   trigger = "focus"
#'                   ),
#'         textInput("importantValue",
#'           tagList("Important Value",
#'             dq_helptag(
#'             "This is an important value, you have to put something in!"
#'             ))
#'       ))
#'     )),
#'   server = function(input, output) {
#'   }
#' )
#'
#' }
dq_helptag <- function(title, trigger = "hover", width = 200, style = NULL) {

  if (length(trigger) == 0 || !(trigger %in% c("hover", "focus"))) {
    trigger <- "hover"
  }

  divC <- paste0("dq-help on-", trigger)

  res <- shiny::tags$div(
    class = divC,
    shiny::icon("question-circle"),
    style = style,
    tabindex = "0",
    shiny::tags$div(
      class = "dq-help-inner",
      shiny::HTML(title),
      style = paste0("width:", shiny::validateCssUnit(width), ";")
    )
  )

  htmltools::attachDependencies(res, init())

}
