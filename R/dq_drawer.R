#' Creates a drawer sidebar element
#'
#' @description Creates a drawer element with buttons to draw out different
#' content elements.
#'
#' @param id optional element id, useful if current state is needed
#' @param ... content elements, must be named since those will be used as button
#' labels
#' @param drawer_style,btn_style optional character specifying additional styles
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
#'       Config = div("Some inputs to configurate things ..."),
#'       Red = div(style = "background: red;width: 100%;height: 100%;"),
#'       "Green Color!" = div("RED!!!!", style = "background: green;"),
#'       direction = "left"
#'     ),
#'     fluidRow(column(3, "Current page:", textOutput("drawerVal"), offset = 5))
#'   ),
#'   server = function(input, output) {
#'     output$drawerVal <- renderText(input$myDrawer)
#'   }
#' )
#'
#' }
dq_drawer <- function(
  id = NULL, ..., drawer_style = NULL, btn_style = NULL,
  direction = c("left", "right", "top", "bottom")
) {
  direction <- match.arg(direction)
  content <- list(...)
  if (!is.list(content) || !length(content) || is.null(names(content))) return()
  content <- content[nzchar(names(content))]
  ns <- shiny::NS("dqdrawer")
  if (is.null(id)) id <- paste0("drawer-", random_id())
  ev <- paste0(
    "$('#", id, "').addClass('dqdrawer-open');",
    "$('#", id, "').find('.dqdrawer-content').removeClass('dqdrawer-in');",
    "event.stopPropagation();"
  )
  c_ids <- structure(as_id(names(content), "dqdrawer"), names = names(content))
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
            ev, "$('#", c_ids[n], "').addClass('dqdrawer-in');",
            "Shiny.setInputValue('", id, "', '", escape(n), "');"
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