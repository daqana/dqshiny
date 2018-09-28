#' Create a HTML table containing the given list of elements
#'
#' @description Creates a HTML table containing the given list of elements.
#' Every element of the list should be a vector of the same length to ensure
#' correct design. The align parameter can be either a character, which will
#' result in all cells having the same alignment, a character vector showing
#' the alignment for every column or a list of character vectors specifying
#' the alignment of every cell.
#'
#' @param elements List of elements to show in the list, each element of the
#'   list should be a row of the table.
#' @param id Optional, character specifying the elements id.
#' @param align Optional, character (vector) or list of characters showing the
#'   alignment of the table or each column/cell, can be one of "left", "right"
#'   or "center", defaults to "left", can be abbreviated.
#' @author richard.kunze
#' @return HTML table
#' @export
#'
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     fluidRow(
#'       column(3, dq_htmltable(list(list("Description", icon("hashtag")),
#'                              list("Value", textInput("value", NULL)),
#'                              list("Result", textOutput("result")))),
#'                              offset = 2, style="background:#ff8f00"
#'       ),
#'       column(3, dq_htmltable(list(list("Left", "Center", "Right"),
#'                              list("Center", "Right", "Left")),
#'                              align = list(c("left", "center", "right"),
#'                              c("center", "right", "left"))),
#'                              offset = 1, style="background:#ff8f00"
#'       )
#'     )),
#'   server = function(input, output) {
#'     observeEvent(input$value, output$result <- renderText(input$value))
#'   }
#' )
#'
#' }
dq_htmltable <- function(elements, id = NULL, align = "left") {

  if (is.null(align)) {
    align <- "left"
  }

  if (length(align) != length(elements[[1]]) || is.list(align)) {
    align <- lapply(align, function(a) { rep_len(a, length(elements[[1]])) })
  }

  if (!is.list(align) || length(align) != length(elements)) {
    align <- lapply(seq(elements), function(x) { unlist(align) })
  }

  align <- fill_alignments(align)
  shiny::tags$table(
    style = "width:100%;", id = id,
    lapply(seq(elements), function(r) {
      shiny::tags$tr(
        lapply(seq(elements[[r]]), function(c) {
          shiny::tags$td(
            elements[[r]][c],
            style = paste0("text-align:", align[[r]][c], ";")
          )
        })
      )
    }),
    init()
  )

}

fill_alignments <- function(align) {

  lapply(align, function(a) {
    a <- gsub("^r$", "right", a)
    a <- gsub("^l$", "left", a)
    gsub("^c$", "center", a)
  })

}
