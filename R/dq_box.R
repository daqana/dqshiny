#' Creates a html box with specified parameters
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
#'
#' @return bootstrap column holding the box
#' @export
#' @import shiny
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     fluidRow(
#'       dq_box(
#'         dq_infobox("hallo", 2, "Welt", icon("hashtag"),
#'                              bg_color = "black", color = "#D00"),
#'         dq_box(
#'           dq_infobox("in the box...", 2, "in the box!", width=12,
#'                                bg_color = "white", color = "#0D0"),
#'           title = "Box in the box", bg_color="pink", width = 8),
#'         title = "Say Hello to my children",
#'         collapsible = TRUE, collapsed = TRUE))
#'   ),
#'   server = function(input, output) {
#'   }
#' )}
dq_box <- function(
  ...,
  id = NULL,
  title = NULL,
  color = "#000", bg_color = "#ff8f00",
  fill = TRUE,
  offset = 0,
  width = 6, height = NULL,
  collapsible = FALSE, collapsed = FALSE
) {

  if (is.null(id)) {
    id <- paste0(sample(letters, 5), collapse = "")
  }

  box_class <- "dq-box"
  box_on_click <- NULL
  body_id <- paste0(sample(letters, 8), collapse = "")
  head_class <- "dq-box-header"
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

  title_tag <- NULL

  if (!is.null(title)) {
    title_tag <- shiny::h3(
      class = "dq-box-title",
      title,
      style = paste0("color:", color, ";")
    )
  }

  body_class <- "dq-box-body"
  body_styles <- NULL

  if (!isTRUE(fill)) {
    body_class <- paste(body_class, "not-filled")
    body_styles <- paste0(
      "border:0 solid ", bg_color, ";",
      "border-width:0 2px 2px;"
    )
  }

  collapse_tag <- NULL

  if (isTRUE(collapsible)) {
    box_on_click <- paste0(
      "document.getElementById('", id, "_collapser').click();"
    )
    body_class <- paste(body_class, "collapse")
    head_class <- paste(head_class, "collapsible")
    if (isTRUE(collapsed)) {
      collapse_icon <- "plus"
    } else {
      collapse_icon <- "minus"
      body_class <- paste(body_class, "in")
    }
    collapse_tag <- shiny::div(
      class = "dq-box-tools",
      shiny::actionButton(
        paste0(id, "_collapser"),
        label = NULL,
        icon = shiny::icon(collapse_icon),
        class = "btn collapser",
        "data-toggle" = "collapse",
        "data-target" = paste0("#", body_id)
      )
    )
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
    dq_dep
  )

}
