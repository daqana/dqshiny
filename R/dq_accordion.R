#' Accordion module to show several collapsible boxes
#'
#' @description Creates an accordion object where one of the contents can be
#' shown and the others will be hidden. The currently activated panel will be
#' available to R over the input$id element.
#'
#' @param id id of the element
#' @param titles character, titles to show in the accordion headers
#' @param contents list of contents, can be character, shiny tags, nested lists
#' of shiny tags ...
#' @param options optional list of jquery-ui options to customize accordions
#' behavior, for a full list of possible options have a look at:
#' \url{http://api.jqueryui.com/accordion}
#' @param sortable optional logical indicating whether the accordion parts
#' should be rearrangeable or not
#' @param bg_color optional character specifying the background color of the
#' headers, can be any valid HTML color code
#' @param hover optional logical indicating whether headers should have an hover
#' effect or not
#' @param style optional character for additional header style attributes
#' @param icons optional named character vector of length one or two indicating
#' the \href{http://fontawesome.io/icons/}{FontAwesome} icons to be used in
#' front of the header title showing the state of the content (open or closed)
#'
#' @return shiny div holding the accordion
#' @export
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' titles <- c("Section 1", "Section 2", "Section 3")
#' contents <- list("Lorem ipsum..", "Lorem ipsum..", tags$p("Lorem ipsum.."))
#' shinyApp(
#'   ui = fluidPage(
#'     fluidRow(
#'       column(5, dq_accordion("myAccordion", titles, contents, hover = FALSE,
#'         style = "border:1px solid red;margin-top: 5px;color: red;"
#'       ), dq_space(),
#'       dq_accordion("myAccordion2", titles, contents,
#'         bg_color = NULL, options = list(animate = 500, collapsible = TRUE),
#'         icons = c(open = "hand-point-down", closed = "hand-point-right")
#'       ), dq_space(),
#'       dq_accordion("myAccordion3", titles, contents,
#'         bg_color = "pink", icons = NULL, sortable = TRUE
#'       ))
#'     )
#'   ), server = function(input, output) {
#'     observeEvent(input$myAccordion, print(input$myAccordion))
#'   }
#' )
#'
#' }
dq_accordion <- function(
  id, titles, contents, options = NULL, sortable = FALSE,
  bg_color = "#ff8f00", hover = TRUE, style = "",
  icons = c(rotate = "angle-right")
) {
  if (length(id) == 0 || length(titles) == 0 || length(contents) == 0) {
    return(NULL)
  }
  requireNamespace("jsonlite")
  if (length(titles) != length(contents)) {
    max_length <- max(length(titles), length(contents))
    contents <- rep_len(contents, max_length)
    titles <- rep_len(titles, max_length)
    warning("Unmatching lengths found, result may not look like desired!")
  }

  acc_class <- paste("dq_accordion", if (!isTRUE(hover)) "noHover")
  icon_dep = NULL
  if (!is.null(icons)) {
    icons <- sapply(icons, function(x) x)
    if (length(icons) == 1) {
      rot <- icons["rotate"]
      if (is.na(rot)) {
        rot <- icons[[1]]
      }
      rot <- sub("^(fa-)?", "fa fa-", rot)
      icons <- list(header = rot, activeHeader = rot)
      acc_class <- paste(acc_class, "dq_accordion_rotated_icon")
    } else if (length(icons) == 2) {
      if (is.null(names(icons)) || is.na(icons["closed"]) || is.na(icons["open"])) {
        warning("Unnamed icon vector found, names (\"closed\",",
                " \"open\") will be used in this order.")
        names(icons) <- c("closed", "open")
      }
      icons <- list(header = sub("^(fa-)?", "fa fa-", icons["closed"]),
                    activeHeader = sub("^(fa-)?", "fa fa-", icons["open"]))
    } else {
      warning("Incorrect icon vector found!")
      return(NULL)
    }
    options$icons <- icons
    icon_dep <- fontawesome_dep
  }

  if (isTRUE(sortable)) {
    options$header <- "> div > h3"
  }

  if (!is.null(bg_color)) {
    style <- paste0("background:", bg_color, ";", style)
  }

  t <- shiny::tags
  t$div(
    id = id,
    lapply(seq(titles), function(i) {
      els <- tagList(
        t$h3(class = acc_class,
             style = style, titles[[i]]),
        t$div(class = "dq_accordion_panel", contents[[i]])
      )
      if (sortable) els <- t$div(class = "group", els)
      els
    }),
    jqueryui_dep,
    htmltools::htmlDependency(
      "accordion", "0.0.1", c(href = "dqshinyRes"),
      stylesheet = "css/accordion.css"
    ),
    icon_dep,
    t$script(accordion_script(id, options, sortable))
  )

}

accordion_script <- function(id, options, sortable) {

  json <- jsonlite::toJSON(options, auto_unbox = TRUE)

  json <- paste0(substr(json, 1, nchar(json) - 1),
                 if (!is.null(options)) ",",
                 "beforeActivate: function(event, ui) {
      if (ui && ui.newHeader[0]) {
        Shiny.onInputChange('", id, "', ui.newHeader[0].textContent);
      }
    }}")

  script <- paste0("$(function(){ $('#", id, "').accordion(", json, ")")
  if (sortable) {
    script <- paste0(
      script, ".sortable({axis: 'y', handle: 'h3', stop: function(event, ui){",
      "ui.item.children('h3').triggerHandler('focusout');",
      "$(this).accordion('refresh');}})"
    )
  }
  shiny::HTML(paste0(script, ";});"))

}
