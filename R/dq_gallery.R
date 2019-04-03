#' Creates a content gallery element
#'
#' @description Creates a gallery element with buttons to switch between
#' several content elements.
#'
#' @param id optional element id, useful if current state is needed
#' @param ... content elements, must be named since those will be used as button
#' labels
#' @param style,content_style optional character specifying additional styles
#' @param arrows optional list of arrows to use, by default defined as list(
#' dq_icon("chevron-left"), dq_icon("chevron-right")), can be a list of other
#' icons or freely designed html elements
#' @param scrollable optional logical to specify if the gallery can be moved
#' with a scrollbar (this option will hide arrows)
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
#'       column(6, dq_gallery(id = "myGallery",
#'         plotOutput("plot1"), plotOutput("plot2"),
#'         plotOutput("plot3"), plotOutput("plot4"),
#'         content_style = "padding: 10px 30px;", arrows = list(
#'           dq_icon("chevron-circle-left"), dq_icon("chevron-circle-right")
#'         )
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
  id = NULL, ..., style = NULL, content_style = NULL, arrows = NULL,
  scrollable = FALSE
) {
  ns <- shiny::NS("dqgallery")
  if (is.null(id)) id <- ns(random_id())
  if (isTRUE(scrollable)) arrows <- NULL
  else arrows <- dq_gallery_arrows(arrows, ns, id)
  div(
    id = id, style = style,
    class = paste(ns(NULL), if (isTRUE(scrollable)) ns("scrollable")),
    arrows[[1L]],
    div(
      class = ns("wrapper"),
      lapply(list(...), div, class = ns("content"), style = content_style)
    ),
    arrows[[2L]],
    init()
  )
}

dq_gallery_arrows <- function(arrows, ns, id) {
  if (is.null(arrows)) arrows <- list(
    dq_icon("chevron-left"), dq_icon("chevron-right")
  )
  list(
    shiny::tagAppendAttributes(
      arrows[[1L]],
      class = paste(ns("arrow"), ns("prev")),
      onclick = paste0("dqUpdateGallery({id:'", id, "', add:", -1L, "})")
    ),
    shiny::tagAppendAttributes(
      arrows[[2L]],
      class = paste(ns("arrow"), ns("next")),
      onclick = paste0("dqUpdateGallery({id:'", id, "', add:", 1L, "})")
    )
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
