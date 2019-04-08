#' Creates an icon element
#'
#' @description Creates a html icon element with the specified icon name from
#' the given library.
#'
#' @param icon name of the icon to show
#' @param lib library used, needed to append the proper dependency
#' @param fa_style fontawesome icon style to use, one of c("fas", "far", "fal")
#' for "solid", "regular" or "light" (PRO only)
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
#' addResourcePath("images", system.file("www", "img", package = "dqshiny"))
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
  icon, lib = "font-awesome", fa_style = c("fas", "far", "fal"),
  color = "#ff8f00", bg_color = NULL, size = NULL, ...
) {
  if (length(size) > 0 && !(size %in% c("xs", "sm", "lg", paste0(2:10, "x")))) {
    size <- NULL
  }
  fa_style <- match.arg(fa_style)
  i_style <- paste0("color:", color, ";background-color:", bg_color, ";")
  dep <- init()
  if (length(icon) && endsWith(icon, ".svg")) {
    class <- paste0("dq-icon", if (length(size)) paste0(" dq-icon-", size))
    el <- shiny::tags$img(src = icon, class = class, style = i_style, ...)
  } else {
    dep <- i_c <- NULL
    if (lib == "font-awesome") {
      i_c <- paste0(fa_style, " fa-", icon)
      if (length(size)) i_c <- paste0(i_c, " fa-", size)
      dep <- append(dep, fontawesome_dep)
    } else if (lib == "glyphicon") {
      i_c <- paste0("glyphicon glyphicon-", icon)
      if (length(size)) i_c <- paste0(i_c, " dq-icon-", size)
    } else {
      warning(paste("Unknown library", lib, "for dq_icon specified!"))
    }
    el <- shiny::tags$i(class = i_c, style = i_style, ...)
  }
  htmltools::attachDependencies(el, dep)
}
