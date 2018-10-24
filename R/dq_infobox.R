#' Creates an info box with given texts
#'
#' @param title Title of the box.
#' @param value Optional, value to show under the title.
#' @param subtitle Optional, subtitle to show under the value.
#' @param icon Optional, icon to show on the left, can be any string, shiny
#'   icon or just NULL to omit it.
#' @param bg_color Optional, sets the background color of the box, can be any
#'   valid html color, standard is dq primary orange.
#' @param color Optional, sets the font color of the box, can be any valid html
#'   color, standard is white.
#' @param width Optional, width of the box measured in bootstrap columns.
#' @param href Optional, link target of the box.
#' @param fill Optional, logical, fill the box with background color or
#'   surround it by box-shadow.
#'
#' @return column holding the info box
#' @export
#' @import shiny
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     fluidRow(
#'       dq_infobox(
#'       "hello", 2, "world", shiny::icon("hashtag"),
#'       bgColor = "black", color = "#D00"
#'       ),
#'       dq_infobox("hello", "2", href="https://www.google.com"),
#'       dq_infobox("hello", 2, "world", "YOU", fill=F)
#'     )
#'   ),
#'   server = function(input, output) {
#'   }
#' )
#'
#' }
dq_infobox <- function(
  title,
  value = NULL,
  subtitle = NULL,
  icon = NULL,
  bg_color = "#FF8F00",
  color = "white",
  width = 4,
  href = NULL,
  fill = TRUE
) {

  d <- shiny::tags$div(
    class = "dq-info-box",
    style = if (isTRUE(fill)) {
      paste0("background-color:", bg_color, ";color:", color)
    } else { "box-shadow: 0 0 3px #666" },
    if (!is.null(icon)) { shiny::tags$span(class = "dq-info-box-icon", icon) },
    shiny::tags$div(
      class = "dq-info-box-content",
      shiny::tags$span(class = "dq-info-box-text", title),
      shiny::tags$span(class = "dq-info-box-number", value),
      shiny::tags$p(subtitle)
    )
  )

  if (!is.null(href)) {
    tl <- shiny::tags$a(class = "dq-info-box-wrapper", href = href, d)
  } else {
    tl <- d
  }

  shiny::column(width, tl, init())

}
