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
#'   list should be a row of the table. Can also be a data.frame with names used
#'   as header.
#' @param id Optional, character specifying the elements id.
#' @param align Optional, character (vector) or list of characters showing the
#'   alignment of the table for each column/cell, can be one of "left", "right"
#'   or "center", defaults to "left", can be abbreviated.
#' @param head_align Optional character vector of header alignments, defaults to
#' "center", can be abbreviated.
#' @param borders Optional character specifying the desired borders to show. Can
#'   be either a single character vector of length one with one of c("inner",
#'   "outer", "all", "tex") or a list of character vectors specifying all
#'   borders by hand. Possible values are "top", "right", "bottom", "left". The
#'   first entry of the list will be used for the header row if given.
#' @author richard.kunze
#' @return HTML table
#' @export
#'
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' lets <- data.frame(lower = letters[1:5], UPPER = LETTERS[1:5])
#' shinyApp(
#'   ui = fluidPage(
#'     dq_space(), fluidRow(
#'       column(3, dq_htmltable(
#'         list(list("Description", icon("hashtag")),
#'              list("Value", textInput("value", NULL)),
#'              list("Result", textOutput("result"))),
#'         borders = "inner"
#'       )),
#'       column(2, dq_htmltable(lets, borders = "outer")),
#'       column(3, dq_htmltable(
#'         list(c("Left", "Center", "Right"), c("Center", "Right", "Left")),
#'         align = list(c("LEFT", "center", "right"), c("c", "R", "l")),
#'         borders = "all"
#'       )),
#'       column(2, dq_htmltable(lets, borders = "tex")),
#'       column(2, dq_htmltable(lets, borders = list(
#'         c("top left", "bottom right"), # header
#'         c("", ""), c("", ""), c("", ""), c("", ""), c("bottom", "bottom")
#'       )))
#'     )),
#'   server = function(input, output) {
#'     output$result <- renderText(input$value)
#'   }
#' )
#'
#' }
dq_htmltable <- function(
  elements, id = NULL,
  align = "left", head_align = "center",
  borders = "none"
) {

  els <- elements
  if (is.data.frame(els)) els <- as.data.frame(t(els))

  header <- names(elements)
  align <- fill_alignments(align, els)
  head_align <- fill_alignments(head_align, header)

  if (is.list(borders)) {
    if (is.null(header)) {
      head_borders <- NULL
      body_borders <- borders
    } else {
      head_borders <- as.list(borders[[1]])
      body_borders <- lapply(borders[-1], as.list)
    }
  } else {
    head_borders <- head_borders(borders, header)
    body_borders <- fill_borders(borders, els)
  }

  shiny::tags$table(
    class = "dq-html-table", id = id,
    if (!is.null(header)) {
      shiny::tags$tr(
        lapply(seq(header), function(i) {
          shiny::tags$th(
            header[[i]], class = paste(head_align[[i]], head_borders[[i]])
          )
        })
      )
    },
    lapply(seq(els), function(r) {
      shiny::tags$tr(
        lapply(seq(els[[r]]), function(c) {
          shiny::tags$td(
            els[[r]][c],
            class = paste(align[[r]][[c]], body_borders[[r]][[c]])
          )
        })
      )
    }),
    init()
  )

}

fill_alignments <- function(align, els) {

  if (is.null(align)) {
    align <- "left"
  }

  if (length(align) != length(els[[1]]) || is.list(align)) {
    align <- lapply(align, rep_len, length(els[[1]]))
  }

  if (!is.list(align) || length(align) != length(els)) {
    align <- lapply(seq(els), function(x) { unlist(align) })
  }

  lapply(align, function(a) {
    a <- gsub("^r$", "right", tolower(a))
    a <- gsub("^l$", "left", a)
    paste0("align-", gsub("^c$", "center", a))
  })

}

fill_borders <- function(type, els) {

  r_max <- length(els)
  c_max <- length(els[[1L]])
  rows <- seq(r_max)
  cols <- seq(c_max)

  if (isTRUE(type == "inner")) {
    lapply(rows, function(r) {
      lapply(cols, function(c) {
        paste(c(
          if (c > 1) "left", if (c < c_max) "right",
          if (r > 1) "top", if (r < r_max) "bottom"
        ), collapse = " ")
      })
    })
  } else if (isTRUE(type == "outer")) {
    lapply(rows, function(r) {
      lapply(cols, function(c) {
        paste(c(
          if (c == 1) "left", if (c == c_max) "right",
          if (r == 1) "top", if (r == r_max) "bottom"
        ), collapse = " ")
      })
    })
  } else if (isTRUE(type == "all")) {
    lapply(rows, function(r) {
      lapply(cols, function(c) {
        paste(c(
          "right", if (c == 1) "left",
          "bottom", if (r == 1) "top"
        ), collapse = " ")
      })
    })
  } else if (isTRUE(type == "tex")) {
    lapply(rows, function(r) {
      lapply(cols, function(c) {
        paste(c(
          if (r == r_max) "bottom", if (r == 1) "top"
        ), collapse = " ")
      })
    })
  } else {
    lapply(rows, function(r) {
      lapply(cols, function(c) { "" })
    })
  }

}

head_borders <- function(type, header) {

  c_max <- length(header)
  cols <- seq(c_max)
  if (isTRUE(type == "inner")) {
    lapply(seq(header), function(c) {
      paste(c(
        if (c > 1) "left", if (c < c_max) "right", "bottom"
      ), collapse = " ")
    })
  } else if (isTRUE(type == "outer")) {
    lapply(seq(header), function(c) {
      paste(c(
        if (c == 1) "left", if (c == c_max) "right", "top", "bottom"
      ), collapse = " ")
    })
  } else if (isTRUE(type == "all")) {
    lapply(seq(header), function(c) {
      paste(c(
        if (c == 1) "left", "right", "top", "bottom"
      ), collapse = " ")
    })
  } else if (isTRUE(type == "tex")) {
    lapply(seq(header), function(c) {
      paste(c(
        "top", "bottom"
      ), collapse = " ")
    })
  } else {
    lapply(seq(header), function(c) { "" })
  }

}
