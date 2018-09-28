#' Function to reset a slider input
#'
#' @description Function to send a message to js to reset an existing slider
#' input. Use it ro restore a slider input's initial values.
#'
#' @param id id of the slider input to reset
#'
#' @return sent message
#' @export
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     sliderInput("mySlider", "Change me", 0, 200, c(90, 117)),
#'     actionButton("btn1", "Reset slider")
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$btn1, reset_slider_input("mySlider"))
#'   }
#' )
#'
#' }
reset_slider_input <- function(id) {
  send_message(type = "resetSlider", ids = id)
}
