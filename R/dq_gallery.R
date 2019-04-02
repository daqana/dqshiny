#' Creates a content gallery element
#'
#' @description Creates a gallery element with buttons to switch between
#' several content elements.
#'
#' @param id optional element id, useful if current state is needed
#' @param ... content elements, must be named since those will be used as button
#' labels
#' @param scrollable optional logical to specify if the gallery can be moved
#' with a scrollbar
#' @param style,content_style optional character specifying additional styles
#' @return gallery element tag
#'
#' @author richard.kunze
#' @export
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     fluidRow(
#'       column(6, dq_gallery(
#'         id = "myGallery",
#'         tags$div(class = "content", plotOutput("plot1")),
#'         tags$div(class = "content", plotOutput("plot2")),
#'         tags$div(class = "content", plotOutput("plot3")),
#'         tags$div(class = "content", plotOutput("plot4"))
#'       )),
#'       column(6,
#'         actionButton("show3", "Show plot 3"),
#'         actionButton("anim", "Animate Plots")
#'       )
#'     )
#'   ),
#'   server = function(input, output) {
#'     output$plot1 <- renderPlot(plot(mtcars$mpg, mtcars$cyl))
#'     output$plot2 <- renderPlot(plot(mtcars$disp, mtcars$hp))
#'     output$plot3 <- renderPlot(plot(mtcars$drat, mtcars$wt))
#'     output$plot4 <- renderPlot(plot(mtcars$qsec, mtcars$vs))
#'
#'     observeEvent(input$show3, update_dq_gallery("myGallery", set = 3))
#'     observeEvent(input$anim, {
#'       for(i in 1:30) {update_dq_gallery("myGallery", add = .1);Sys.sleep(.2)}
#'     })
#'   }
#' )
#'
#' }
dq_gallery <- function(
  id = NULL, ..., scrollable = FALSE, style = NULL, content_style = NULL
) {
  ns <- shiny::NS("dqgallery")
  if (is.null(id)) id <- ns(random_id())
  div(
    id = id, style = style,
    class = paste(ns(NULL), if (isTRUE(scrollable)) ns("scrollable")),
    if (!isTRUE(scrollable)) tags$span(
      class = paste(ns("arrow"), ns("prev")),
      onclick = paste0("dqUpdateGallery({id:'", id, "', add:", -1L, "})")
    ),
    div(
      class = ns("wrapper"),
      lapply(list(...), div, class = ns("content"), style = content_style)
    ),
    if (!isTRUE(scrollable)) tags$span(
      class = paste(ns("arrow"), ns("next")),
      onclick = paste0("dqUpdateGallery({id:'", id, "', add:", 1L, "})")
    ),
    init()
  )
}

#' @param set index of the content element to show
#' @param add numeric amount of indexes to move (can also be negative)
#'
#' @export
#' @rdname dq_gallery
update_dq_gallery <- function(id, set = NULL, add = NULL) {
  send_message(
    "dqUpdateGallery", id, set = if (!is.null(set)) set - 1L, add = add
  )
}
