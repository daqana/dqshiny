#' @title Initializes dqshiny
#'
#' @description Can be used inside the shinyUI to add dq shiny resource path.
#' Will be automatically called by loading the package and using any of the
#' dqshiny elements.
#'
#' @return dqshiny CSS and JS dependency
#' @export
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init()
#'   ),
#'   server = function(input, output) {}
#' )
#'
#' }
init <- function() {
  shiny::singleton(shiny::addResourcePath(
    "dqshinyRes", system.file("www", package = "dqshiny")
  ))
  list(dq_dep)
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
