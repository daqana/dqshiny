#' Function to (re)render an existing rhandsontable
#'
#' @description Function to send a message to js to render an existing
#' rhandsontable object. E.g. needed when handsontable is used with stretchH =
#' 'all' and a select callback to choose a dataset and present its data to the
#' user. Handsontable won't properly render the table if the height of the page
#' increases due to some visibility changes and a scroll bar appears.
#'
#' @param id id of the hot object
#'
#' @return sent message
#' @author richard.kunze
#' @export
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(rhandsontable)
#' shinyApp(
#'   ui = fluidPage(
#'     dq_handsontable_output("random", 9),
#'     actionButton("render", "Render HoT"),
#'     fluidRow(id="bigRow", class="hidden",
#'       style="height:100vh;background:#ff8f00;")
#'   ),
#'   server = function(input, output) {
#'     hw <- c("Hello", "my", "funny", "world!")
#'     data <- data.frame(A=hw, B=hw[c(2,3,4,1)], C=1:4, D=Sys.Date() - 0:3,
#'       stringsAsFactors = FALSE)
#'     dq_render_handsontable("random", data, "rand",
#'       filters = c("S", "T", "R", "R"),
#'       table_param = list(rowHeaders = NULL, selectCallback = TRUE))
#'     observeEvent(input$random_select, toggle("bigRow"))
#'     observeEvent(input$render, render_hot("random"))
#'   }
#' )
#'
#' }
render_hot <- function(id) {
  send_message(type = "renderHot", ids = id)
}
