#' @title Creates an autocomplete text input field
#'
#' @description autocomplete_input creates an autocomplete text input field,
#' showing all possible options from a given list under the input while typing.
#' Alternative to very slow select(ize) inputs for (very) large option lists.
#'
#' @param id id of the element
#' @param label label to show for the input, NULL for no label
#' @param options list (or vector) of possible options
#' @param value initital value
#' @param width optional, the width of the input, see
#' \code{\link[shiny:validateCssUnit]{validateCssUnit}}
#' @param placeholder optional character specifying the placeholder text
#' @param max_options optional numeric specifying the maximum number of
#' options to show (for performance reasons)
#' @param hide_values optional boolean indicating whether to show values
#' under labels or not
#'
#' @return autocomplete_input: shiny input element
#'
#' @export
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' opts <- sapply(1:100000, function(i) paste0(sample(letters, 9), collapse = ""))
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     fluidRow(
#'       column(3, autocomplete_input("auto1", "Unnamed:", opts, maxOptions = 1000),
#'                 autocomplete_input("auto2", "Named:", maxOptions = 1000,
#'                   structure(opts, names = opts[order(opts)])),
#'                 autocomplete_input("auto3", "Big data:", maxOptions = 1000, NULL,
#'                   placeholder = "Big data taking several seconds to load ..."),
#'                 actionButton("calc", "Calculate")
#'       ), column(3, tags$label("Value:"), verbatimTextOutput("auto1_val", placeholder = TRUE),
#'                 tags$label("Value:"), verbatimTextOutput("auto2_val", placeholder = TRUE))
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     output$auto1_val <- renderText(as.character(input$auto1))
#'     output$auto2_val <- renderText(as.character(input$auto2))
#'     observeEvent(input$calc, {
#'       Sys.sleep(3)
#'       update_autocomplete_input(session, "auto3", placeholder = "Loaded!",
#'         options = rownames(mtcars))
#'     })
#'   }
#' )}
autocomplete_input <- function(
  id, label, options, value = "", width = NULL,
  placeholder = NULL, max_options = 0, hide_values = FALSE
) {
  value <- shiny::restoreInput(id = id, default = value)
  js_opts <- jsonlite::toJSON(as.list(options), auto_unbox = TRUE)
  width <- shiny::validateCssUnit(width)
  shiny::div(
    class = "form-group shiny-input-container autocomplete",
    style = if (!is.null(width)) paste0("width: ", width, ";"),
    if (!is.null(label)) shiny::tags$label(label, `for` = id),
    shiny::tags$input(
      id = id, type = "text", class = "form-control", result = value,
      value = value, placeholder = placeholder, "data-options" = js_opts,
      "data-max" = max_options, "data-hide" = isTRUE(hide_values)
    ),
    htmltools::htmlDependency(
      "autocomplete", "0.0.1", c(href = "dqshinyRes"),
      script = "js/autocomplete-binding.js", stylesheet = "css/autocomplete.css"
    )
  )
}

#' @description update_autocomplete_input changes the value or the options of an
#' autocomplete input element on the client side.
#'
#' @param session the shiny session object
#'
#' @return update_autocomplete_input: message to the client
#' @export
#' @rdname autocomplete_input
update_autocomplete_input <- function(
  session, id, label = NULL, options = NULL, max_options = NULL,
  value = NULL, placeholder = NULL, hide_values = NULL
) {
  message <- shiny:::dropNulls(list(
    label = label, options = options, value = value, maxOptions = max_options,
    placeholder = placeholder, hideValues = hide_values
  ))
  session$sendInputMessage(id, message)
}
