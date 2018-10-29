#' @title Create a dq box group
#'
#' @description Create a dq box group which automatically collapses all other
#' boxes whenever one box is opened.
#'
#' @param session shiny session object
#' @param ... box ids to be combined to one group
#'
#' @export
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     fluidRow(
#'       column(6,
#'         dq_box("Random Input1", dq_space(), "End of Content", id = "box1",
#'           title = "Box1", width = 12, open_callback = TRUE),
#'         dq_box("Random Input2", dq_space(), "End of Content", id = "box2",
#'           title = "Box2", width = 9, collapsed = TRUE, open_callback = TRUE),
#'         dq_box("Random Input3", dq_space(), "End of Content", id = "box3",
#'           title = "Box3", width = 12, collapsed = TRUE, open_callback = TRUE)
#'       )
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     create_dq_box_group(session, "box1", "box2", "box3")
#'   }
#' )
#'
#' }
create_dq_box_group <- function(session, ...) {
  bL <- list(...)
  if (length(bL) == 0) return()
  if (length(bL) == 1 && length(bL[[1]]) > 1) bL <- bL[[1]]
  input <- session$input
  lapply(bL, function(b) {
    tmp <- paste0(b, "_open")
    shiny::observeEvent(input[[tmp]], {
      if (!input[[tmp]]) return()
      lapply(bL, function(b2) {
        if (b2 != b) update_dq_box(b2, TRUE)
      })
    })
  })
}

#' @title Directly render a set of dq_boxes as a group
#'
#' @description Directly render a set of dq_boxes into an uiOutput element. All
#' given dq_boxes will be prepared automatically, meaning that they become
#' 'collapsible' and 'open_callback'ed.
#'
#' @param ... a set of dq_boxes
#' @param open optional integer or character of length one, specifying the
#' initially opened box
#'
#' @return fluidRow containing the grouped dq_boxes
#' @export
#'
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     fluidRow(column(6, uiOutput("myGroup")))
#'   ),
#'   server = function(input, output) {
#'     output$myGroup <- render_dq_box_group(
#'       dq_box("Random Input1", dq_space(), "End of Content",
#'         title = "TestBox1", width = 12),
#'       dq_box("Random Input2", dq_space(), "End of Content",
#'         title = "TestBox2", width = 9),
#'       dq_box("Random Input3", dq_space(), "End of Content",
#'         title = "TestBox3", width = 12),
#'       open = 3L)
#'   }
#' )
#'
#' }
render_dq_box_group <- function(..., open = NULL) {
  bL <- list(...)
  if (length(bL) > 0) {
    ids <- vapply(bL, function(b) b$attribs$id, "")
    bL <- lapply(seq(length(bL)), function(i) {
      b <- bL[[i]]
      groupify_dq_box(b, length(open) == 1 && (i == open || ids[[i]] == open))
    })
    create_dq_box_group(shiny::getDefaultReactiveDomain(), ids)
  }
  shiny::renderUI(
    shiny::fluidRow(bL)
  )
}

#' @author richard.kunze
groupify_dq_box <- function(wrapper, open) {
  id <- wrapper$attribs$id
  box <- wrapper$children[[1]]
  if (is.null(box$children[[1]])) return(wrapper)
  if (is.null(box$children[[1]]$onclick)) {
    box$children[[1]] <- shiny::tagAppendAttributes(
      box$children[[1]], onclick = paste0(
        "document.getElementById('", id, "_collapser').click();"
      )
    )
  }
  box_class <- box$children[[1]]$attribs$class
  if (!grepl("collapsible", box_class, fixed = TRUE)) {
    box$children[[1]]$attribs$class <- paste(box_class, "collapsible")
  }
  body_id <- box$children[[2]]$attribs$id
  collapse_tag <- box$children[[1]]$children[[2]]
  if (is.null(collapse_tag)) {
    box$children[[1]]$children[[2]] <- create_collapse_tag(!open, id, body_id)
  }
  body_class <- box$children[[2]]$attribs$class
  box$children[[2]]$attribs$class <- paste(
    gsub("collapse( in)?", "", body_class), "collapse", if (open) "in"
  )
  wrapper$children[[1]] <- box
  wrapper
}
