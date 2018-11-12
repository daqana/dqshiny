#' @title Creates a time input field
#'
#' @description time_input creates a time input field for correctly formatted
#' time values
#'
#' @param id id of the element
#' @param label label to show for the input, NULL for no label
#' @param value initial value
#' @param min minimum time value, must follow the specified format, e.g. "08:00"
#' @param max maximum time value, must follow the specified format, e.g. "17:00"
#' @param format format to use for the time string, can be any valid
#' \href{http://momentjs.com/docs/#/displaying/format/}{moment.js} time
#' format (NOTE: this will only work with the material time picker and can't be
#' updated!)
#' @param placeholder optional character specifying the placeholder text
#' @param width optional, the width of the input, see
#' \code{\link[shiny:validateCssUnit]{validateCssUnit}}
#' @param color color of the watch hand (of material time picker)
#' @param use_material_picker boolean to specify if the input should be a
#' simple time (text) input or use the bootstrap material time picker
#'
#' @return time_input: shiny input element
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
#'       column(3,
#'         time_input("time1", "Simple:", value = "12:34"),
#'         time_input("time2", "Fancy:", use_material_picker = TRUE),
#'         actionButton("update", "Update")
#'       )
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     observeEvent(input$update, {
#'       update_time_input(session, "time2", value = "12:34")
#'     })
#'   }
#' )
#'
#' }
time_input <- function(
  id, label, value = "", min = NULL, max = NULL,
  format = "HH:mm", placeholder = NULL, width = NULL, color = NULL,
  use_material_picker = FALSE
) {
  input_dep <- htmltools::htmlDependency(
    "timeInput", "0.0.1", list(href = "dqshinyRes"),
    script = "js/time-input-binding.js"
  )
  deps <- init()
  type <- "time"
  width <- shiny::validateCssUnit(width)
  if (use_material_picker) {
    type <- "text"
    deps <- append(deps, list(
      htmltools::htmlDependency(
        "moment", "2.22.2", list(href = "dqshinyRes"),
        script = "js/moment.min.js"
      ),
      htmltools::htmlDependency(
        "timePicker", "2.7.1", list(href = "dqshinyRes"),
        script = "js/bootstrap-material-datetimepicker.min.js",
        stylesheet = "css/bootstrap-material-datetimepicker.min.css"
      )
    ))
  }
  deps <- append(deps, list(input_dep))
  shiny::div(
    style = if (!is.null(width)) paste0("width: ", width, ";"),
    if (!is.null(label)) shiny::tags$label(label, `for` = id),
    class = "form-group shiny-bound-input shiny-input-container time-input",
    shiny::tags$input(
      id = id, class = "form-control", value = value, type = type, min = min,
      max = max, format = format, placeholder = placeholder, color = color,
      ui = if (use_material_picker) "material" else "standard"
    ),
    deps
  )
}

#' @description update_time_input changes the value/label or placeholder of an
#' time input element on the client side.
#'
#' @param session the shiny session object
#'
#' @return update_time_input: message to the client
#' @export
#' @rdname time_input
update_time_input <- function(
  session, id, label = NULL, value = NULL,
  min = NULL, max = NULL, placeholder = NULL
) {
  message <- not_null(list(
    label = label, value = value, min = min,
    max = max, placeholder = placeholder
  ))
  session$sendInputMessage(id, message)
}
