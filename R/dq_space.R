#' Create an empty div for spacing
#'
#' @description Creates an empty div with the desired height
#' clearing the space on the desired sites.
#'
#' @param height Height of the space, can be any valid CSS unit,
#'   validation will be done with \code{\link{validateCssUnit}}.
#' @param clear Optional, can be one of 'both', 'left', 'right' and 'none'
#'   to specify which elements should be cleared, will be 'both'
#'   if given value is omitted or none of these.
#'
#' @return shiny div for spacing
#' @export
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     fluidRow(
#'       dq_box(title = "I need space!!", dqSpace(200)),
#'       dq_box(
#'       title = "I need more space", collapsible = TRUE, collapsed = TRUE,
#'             "...but I'm collapsible!", dqSpace("50vh")
#'             )
#'     )),
#'   server = function(input, output) {
#'   }
#' )
#'
#' }
dq_space <- function(height = 30, clear = "both") {

  if (length(clear) == 0 || !(clear %in% c("both", "left", "right", "none"))) {
    clear <- "both"
  }

  shiny::tags$div(
    style = paste0(
      "height:", shiny::validateCssUnit(height), ";",
      "clear:", clear, ";"
    ),
    init()
  )

}
