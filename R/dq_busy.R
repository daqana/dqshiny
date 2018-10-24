#' Adds a loading image if shiny is busy
#'
#' @description Adds a loading image to the page which is visible when shiny is
#' busy.
#'
#' @param icon_path optional character, icon source path
#' @param time optional integer indicating the animation time of the loader.
#' Can be useful to omit loader for many short loading moments, but show it if
#' calculation really needs some time. Set to 0 to disable animations.
#' @param animation optional character specifying the animation of the loader
#'
#' @return shiny tag holding the icon
#' @export
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     dq_busy(time = 1500),
#'     actionButton("button", "make me busy")
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$button, Sys.sleep(3))
#'   }
#' )
#'
#' }
dq_busy <- function(icon_path = NULL, time = 500, animation = "fadeIn") {

  if (is.null(icon_path)) icon_path <- "dqshinyRes/img/busy.gif"
  if (!is.numeric(time)) time <- 500
  busy_styles <- paste0(
    "position:fixed;top:50%;left:50%;margin:-16px;width:32px;z-index:999999;",
    "-webkit-animation:", animation, " ", time, "ms ease-in forwards;",
    "animation:", animation, " ", time, "ms ease-in forwards;")
  shiny::conditionalPanel(
    "$('html').hasClass('shiny-busy')",
    shiny::tags$img(style = busy_styles, src = icon_path),
    init(),
    htmltools::htmlDependency("transitions", "0.0.1", c(href = "dqshinyRes"),
                              stylesheet = "css/transitions.min.css")
  )

}
