#' @title Initializes dqshiny styling
#' @description Must be used inside the shinyUI before any other daqana styling function is used.
#' Specify parameters to automatically add content to your side. Please notice, that no additional parameters can be given in this case.
#'
#' @return list of shiny includes executed by shinyUI
#' @export
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     dq_header()
#'   ),
#'   server = function(input, output) {}
#' )}
init <- function() {
  shiny::singleton(shiny::addResourcePath(
    "dqshinyRes", system.file("www", package = "dqshiny")
  ))
  NULL
}

#' @title Initializes dqshiny fonts for figures
#' @description Only needed for highcharter/ggplot2 figures
#' @param paths vector of paths to look in
#' @param pattern pattern to th files have to match
#' @param silent optional logical indicating whether message and warnings should be printed or not
#' @return NULL
#' @export
#' @author david.breuer
init_fonts <- function(paths, pattern = "ttf$", silent = TRUE) {
  requireNamespace("extrafont", quietly = TRUE)
  fonts <- extrafont::fonttable()$FamilyName
  invisible(lapply(paths, function(p) {
    files <- list.files(p, pattern)
    lapply(files, function(f) {
      name <- gsub("\\..*$", "", f)
      if (length(grep(name, fonts)) == 0) {
        func <- function() {
          extrafont::font_import(paths = p, pattern = f, prompt = FALSE)
        }
        if (silent) {
          suppressWarnings(suppressMessages(func()))
        } else {
          func()
        }
      }
    })
  }))
}
