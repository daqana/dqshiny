#' Sets the initial state of all given tags
#'
#' @description Hidden only hides elements on the top level, so showing these elements will also show all children of them.
#'
#' @param ... tags to add to the ui, can be a single element or nested tagLists
#'
#' @return tags with the state change
#' @export
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     actionButton("btn1", "Press Me"),
#'     hidden(disabled(actionButton("btn2", "Show")))
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$btn1, toggle("btn2"))
#'   }
#' )}
hidden <- function(...) {
  tl <- list(...)
  if (length(tl) == 1) {
    tag <- tl[[1]]
    if (inherits(tag, "shiny.tag")) {
      shiny::tagAppendAttributes(tag, class = "hidden")
    } else if (is.list(tag)) {
      lapply(tag, hidden)
    } else {
      tag
    }
  } else {
    lapply(tl, hidden)
  }
}

#' @description Disabled will recursively traverse the given elements and its children and set all inputs, buttons, selects and textfields
#' to be disabled.
#'
#' @export
#' @rdname hidden
disabled <- function(...) {
  tl <- list(...)
  if (length(tl) == 1) {
    tag <- tl[[1]]
    if (inherits(tag, "shiny.tag")) {
      tagCl <- tag$attribs$class
      if (!is.null(tagCl) && grepl("shiny-date-input", tagCl)) {
        tag$attribs$class <- paste(tagCl, "disabled-date-input")
      } else if (tag$name %in% c("textarea", "input", "button", "select")) {
        if (!is.null(tagCl) && tagCl == "js-range-slider") {
          tag <- shiny::tagAppendAttributes(tag, "data-disable" = T)
        } else {
          tag <- shiny::tagAppendAttributes(tag, disabled = "")
        }
      } else if (length(tag$children) > 0) {
        tag$children <- lapply(tag$children, disabled)
      }
      tag
    } else if (is.list(tag)) {
      lapply(tag, disabled)
    } else {
      tag
    }
  } else {
    lapply(tl, disabled)
  }
}
