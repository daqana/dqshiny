#' Creates a html box with specified parameters
#'
#' @description Creates a fully customizable HTML box holding the given content.
#' Can be made collapsible and nested.
#'
#' @param ... Tags to add to the box as children.
#' @param id ID of the box, can be used to observe collapse events via
#'   input[[paste0(id, "_collapser")]].
#' @param title Title of the box, always visible.
#' @param color Optional, sets the font color of the box, can be any valid
#'   html color, defaults to black.
#' @param bg_color Optional, sets the background color of the box, can be any
#'   valid html color, defaults to dq primary orange.
#' @param fill Optional logical, decide whether to fill the body with
#'   background color or not, if not, a border of this color will be set
#'   instead.
#' @param offset Optional, offset of the resulting column measured in bootstrap
#'   columns.
#' @param width Optional, width of the box measured in bootstrap columns.
#' @param height Optional, height of the box, can be numeric, which will result
#'   in pixels, or measured in any valid CSS3 length format, if the given body
#'   is larger than this value overflow will be set to auto, resulting in a
#'   scrollable body.
#' @param collapsible Optional logical, whether the box is collapsible or not.
#' @param collapsed Optional logical, whether the box is initially collapsed or
#'   not.
#' @param open_callback optional logical, whether to send messages whenever the
#' state of the box changes or not, events will be available via
#' input[[paste0(id, "_open")]]
#'
#' @return bootstrap column holding the box
#' @export
#' @import shiny
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     fluidRow(
#'       dq_box(
#'         title = "Say Hello to my children", id = "bigBox", collapsed = TRUE,
#'         dq_infobox("hallo", 2, "Welt", icon("hashtag"),
#'                    bg_color = "black", color = "#D00"),
#'         dq_box(
#'           title = "Box in the box", bg_color = "pink", width = 8,
#'           dq_infobox("in the box...", 2, "in the box!", width = 12,
#'                      bg_color = "white", color = "#0D0")
#'           )
#'         ),
#'       column(3, actionButton("toggle", "Toggle Box"))
#'      )
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$toggle, update_dq_box("bigBox"))
#'   }
#' )
#'
#' }
dq_box <- function(
  ..., id = NULL, title = NULL,
  color = "#000", bg_color = "#ff8f00", fill = TRUE,
  width = 6L, height = NULL, offset = 0L,
  collapsible = FALSE, collapsed = FALSE, open_callback = FALSE
) {

  if (is.null(id)) {
    id <- paste0(sample(letters, 5L), collapse = "")
  }

  box_class <- "dq-box"
  box_on_click <- NULL
  body_id <- paste0(sample(letters, 8L), collapse = "")
  head_class <- "dq-box-header clearfix"
  box_styles <- ""

  if (!is.null(bg_color)) {
    box_styles <- paste0(box_styles, "background:", bg_color, ";")
  }

  if (!is.null(height)) {
    box_styles <- paste0(
      box_styles,
      "height:", shiny::validateCssUnit(height), ";",
      "overflow:auto;"
    )
  }

  title_tag <- create_box_title(title, color)

  body_class <- "dq-box-body clearfix"
  body_styles <- NULL

  if (!isTRUE(fill)) {
    body_class <- paste(body_class, "not-filled")
    body_styles <- paste0(
      "border:0 solid ", bg_color, ";",
      "border-width:0 2px 2px;"
    )
  }

  collapse_tag <- NULL
  box_on_click <- NULL
  if (isTRUE(collapsible) || isTRUE(collapsed) || isTRUE(open_callback)) {
    box_on_click <- paste0(
      "document.getElementById('", id, "_collapser').click();"
    )
    body_class <- paste(body_class, "collapse", if (!collapsed) "in")
    head_class <- paste(head_class, "collapsible")
    collapse_tag <- create_collapse_tag(collapsed, id, body_id, open_callback)
  }

  header_tag <- NULL
  if (!is.null(title_tag) || !is.null(collapse_tag)) {
    header_tag <- shiny::div(
      class = head_class, title_tag, collapse_tag, onClick = box_on_click
    )
  } else {
    box_class <- paste(box_class, "no-header")
  }

  shiny::column(
    width,
    id = id,
    class = "dq-box-wrapper",
    offset = offset,
    shiny::div(
      class = box_class, style = box_styles,
      header_tag,
      shiny::div(id = body_id, class = body_class, style = body_styles, ...)
    ),
    init()
  )

}

create_collapse_tag <- function(collapsed, id, body_id, open_callback = TRUE) {
  coll_icon <- if (collapsed) "plus" else "minus"
  btn <- shiny::actionButton(
    paste0(id, "_collapser"), label = NULL, icon = shiny::icon(coll_icon),
    class = "btn collapser", "data-toggle" = "collapse",
    "data-target" = paste0("#", body_id)
  )
  if (open_callback) {
    on_click <- paste0(
      "Shiny.onInputChange('", id, "_open', !$(document.getElementById('",
      body_id, "')).hasClass('in'));"
    )
    btn <- shiny::tagAppendAttributes(btn, onclick = on_click)
  }
  shiny::div(class = "collapser-wrapper", btn)
}

create_box_title <- function(title, color) {
  if (!is.null(title)) {
    return(shiny::h3(
      title, class = "dq-box-title", style = paste0("color:", color, ";")
    ))
  }
  NULL
}


#' Function to update the collapsed status of a dqBox
#'
#' @description Function to update the collapsed state of a dq_box.
#'
#' @param silent optional logical indicating to suppress events or not
#'
#' @export
#' @rdname dq_box
update_dq_box <- function(id, collapsed = NULL, silent = FALSE) {
  send_message(
    type = "updateBox", ids = id, collapsed = collapsed, silent = silent
  )
}
