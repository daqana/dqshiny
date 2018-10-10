#' @title Render ggplot2 figure as svg
#' @description Returns ggplot2 svg image for shiny::imageOutput
#' @param gg reactive or non-reactive ggplot2 object
#' @param path directory where images are stored (optional, default = NULL, creates tmp files)
#' @param width plot width (optional, default = 1200)
#' @param height plot height (optional, default = 500)
#' @param alt alternative image title (optional, default = "")
#' @param pdf boolean variable controlling if pdf output is generated (optional, default = FALSE)
#' @param png boolean variable controlling if png output is generated (optional, default = FALSE)
#' @param delete_file parameter for shiny::renderImage function
#' @return list containing ggplot2 figure information
#' @family plotting
#' @author david.breuer
#' @export
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     selectInput("select","Number of bars", choices=c(4,5,6)),
#'     br(),
#'     imageOutput("plot")
#'   ),
#'   server = function(input, output) {
#'     gf <- reactive({L <- as.integer(input$select)
#'                     gg <- ggplot2::ggplot(data=data.frame(x=seq(L), y=seq(L)),
#'                       ggplot2::aes(x=x, y=y)) + ggplot2::geom_bar(stat = "identity")
#'     })
#'     output$plot <- dq_render_svg(gf)
#'   }
#' )
#'
#' }
dq_render_svg <- function(gg, path = NULL, width = 1200, height = 500, alt = "", pdf = FALSE, png = FALSE, delete_file = TRUE) {
  requireNamespace("ggplot2")
  rf <- shiny::reactive({
    if (!is.null(gg)) {
      if (shiny::is.reactive(gg)) {
        out_svg <- save_plot(gg(), path = path, width = width, height = height, pdf = pdf, png = png)
      } else {
        out_svg <- save_plot(gg, path = path, width = width, height = height, pdf = pdf, png = png)
      }
    } else {
      out_svg <- path
    }
    list(src = normalizePath(out_svg), contentType = "image/svg+xml",
         width = width, height = height, alt = alt)
  })
  shiny::renderImage(rf(), deleteFile = delete_file)
}

#' @author david.breuer
save_plot <- function(gg, path = NULL, width = 1200, height = 500, pdf = TRUE, png = FALSE) {
  requireNamespace("ggplot2")
  if (is.null(path)) {
    out_svg <- tempfile(fileext = ".svg")
  } else {
    out_svg <- path
  }
  out_pdf <- gsub(".svg", ".pdf", out_svg)
  out_png <- gsub(".svg", ".png", out_svg)
  suppressMessages({
    ggplot2::ggsave(file = out_svg, plot = gg, width = width / 72, height = height / 72, device = "svg")
    if (pdf) ggplot2::ggsave(file = out_pdf, plot = gg, width = width / 90, height = height / 90, device = "pdf")
    if (png) ggplot2::ggsave(file = out_png, plot = gg, width = width / 300, height = height / 300, device = "png")
  })
  out_svg
}
