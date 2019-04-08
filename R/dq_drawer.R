#' Creates a drawer sidebar element
#'
#' @description Creates a drawer element with buttons to draw out different
#' content elements.
#'
#' @param id optional element id, useful if current state is needed
#' @param ... content elements, must be named since those will be used as button
#' labels, can also be a named list
#' @param drawer_style,btn_style optional character specifying additional styles
#' @param size optional size of the drawer (width or height depending on
#' direction), can be any valid CSS unit (see
#' \code{\link[shiny:validateCssUnit]{validateCssUnit}})
#' @param direction optional, specifies the direction the drawer comes from, one
#' of c("left", "right", "top", "bottom)
#' @return drawer element tag
#'
#' @author richard.kunze
#' @export
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     dq_drawer(
#'       id = "myDrawer",
#'       Config = div(actionButton("hide", "Close drawer")),
#'       Red = div(style = "background: red;width: 100%;height: 100%;"),
#'       "Green Color!" = div("RED!!!!", style = "background: green;"),
#'       direction = "left", size = 250
#'     ),
#'     fluidRow(column(
#'       3, offset = 5,
#'       "Current page:", textOutput("drawerVal"),
#'       actionButton("show", "Show Green")
#'     ))
#'   ),
#'   server = function(input, output) {
#'     output$drawerVal <- renderText(input$myDrawer)
#'     observeEvent(input$show, update_dq_drawer("myDrawer", "Green Color!"))
#'     observeEvent(input$hide, update_dq_drawer("myDrawer", NULL))
#'   }
#' )
#'
#' }
dq_drawer <- function(
  id = NULL, ..., drawer_style = NULL, btn_style = NULL, size = NULL,
  direction = c("left", "right", "top", "bottom")
) {
  direction <- match.arg(direction)
  content <- list(...)
  if (!length(content)) return()
  if (!is.list(content) || is.null(names(content))) {
    content <- content[[1L]]
    if (!is.list(content) || is.null(names(content))) return()
  }
  content <- content[nzchar(names(content))]
  ns <- shiny::NS("dqdrawer")
  if (is.null(id)) id <- ns(random_id())
  c_ids <- structure(as_id(names(content), "dqdrawer"), names = names(content))
  size <- tryCatch(shiny::validateCssUnit(size), error = function(e) NULL)
  if (!is.null(size)) {
    wh <- if (direction %in% c("left", "right")) "width:" else "height:"
    drawer_style <- paste0(
      wh, size, ";", direction, ":-", size, ";", drawer_style
    )
  }
  shiny::div(
    id = id,
    class = paste(ns("wrapper"), ns(direction)),
    onclick = paste0("$('#", id, "').removeClass('dqdrawer-open');"),
    shiny::div(
      class = ns("container"),
      style = drawer_style,
      onclick = "event.stopPropagation();",
      shiny::div(
        class = ns("button-wrapper"),
        lapply(names(content), function(n) {
          n_ev <- paste0(
            "dqUpdateDrawer({id:'", id, "',open:'", c_ids[n], "',value:'",
            escape(n), "'});event.stopPropagation();"
          )
          shiny::div(class = ns("button"), n, onclick = n_ev, style = btn_style)
        })
      ),
      lapply(names(content), function(n) {
        shiny::div(id = c_ids[n], class = ns("content"), content[[n]])
      })
    ),
    init()
  )
}

#' @param open name of the content element to show, NULL to close drawer
#'
#' @export
#' @rdname dq_drawer
update_dq_drawer <- function(id, open) {
  send_message(
    "dqUpdateDrawer", id, value = open,
    open = if (is.null(open)) open else as_id(open, "dqdrawer")
  )
}
