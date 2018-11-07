#' Creates an icon element
#'
#' @description Creates a html icon element with the specified icon name from
#' the given library.
#'
#' @param icon name of the icon to show
#' @param lib library used, needed to append the proper dependency
#' @param color icon color, can be any valid CSS color code
#' @param bg_color icon background color, can be any valid CSS color code
#' @param size character specifying the size of the icon, can be one of "xs",
#' "sm", "lg", "2x", "3x", "4x", "5x", "6x", "7x", "8x", "9x", "10x"
#' @param ... additional attributes like style or class
#'
#' @return icon html element
#' @export
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' addResourcePath("images", system.file("www", "img", package = "dqutils"))
#' shinyApp(
#'   ui = fluidPage(
#'     dq_icon("table", size = "4x"),
#'     dq_icon("check", color = "red", lib = "glyphicon", size = "2x"),
#'     dq_icon("phone", bg_color = "green", size = "lg"),
#'     dq_icon("images/logo_daqana.svg", size = "3x")
#'   ),
#'   server = function(input, output, session) {}
#' )
#'
#' }
dq_icon <- function(
  icon, lib = "font-awesome",
  color = "#ff8f00", bg_color = "#fff", size = NULL, ...
) {
  if (length(size) > 0 && !(size %in% c("xs", "sm", "lg", paste0(2:10, "x")))) {
    size <- NULL
  }
  style <- paste0("color:", color, ";background-color:", bg_color, ";")
  dep <- init()
  if (length(icon) > 0 && endsWith(icon, ".svg")) {
    class <- paste0("dq-icon", if (length(size) > 0) paste0(" dq-icon-", size))
    el <- shiny::tags$img(src = icon, class = class, style = style, ...)
  } else {
    dep <- NULL
    pre <- "glyphicon"
    sPre <- "dq-icon"
    if (lib == "font-awesome") {
      pre <- sPre <- "fa"
      dep <- append(dep, fontawesome_dep)
    } else if (lib != "glyphicon") {
      warning(paste("Unknown library", lib, "for dq_icon specified!"))
    }
    class <- paste0(pre, " ", pre, "-", icon, if (length(size) > 0) paste0(" ", sPre, "-", size))
    el <- shiny::tags$i(class = class, style = style, ...)
  }
  htmltools::attachDependencies(el, dep)
}
