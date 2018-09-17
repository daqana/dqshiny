#' @title Creates a state button showing different icons
#'
#' @description Creates a state button showing different states by different
#' icons.
#'
#' @param id id of the element
#' @param states character of possible states, must be valid fontAwesome icon
#' names \code{\link[shiny:icon]{icon}}
#' @param value optional value, can be integer position in states or
#' character giving the state, must be of length one
#' @param ... additional parameters like css classes or styles
#'
#' @return icon_state_button: shiny input element
#'
#' @export
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' hands <- paste0("hand-o-", c("up", "right", "down", "left"))
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     fluidRow(column(12,
#'       icon_state_button("sort", c("sort", "sort-up", "sort-down")),
#'       icon_state_button("hands", hands, 1),
#'       icon_state_button("mood", c("smile-o", "meh-o", "frown-o"), "smile-o"),
#'       br(), actionButton("makeStars", "I like stars")
#'     ))
#'   ),
#'   server = function(input, output, session) {
#'     observeEvent(input$makeStars, update_icon_state_button(
#'       session, "mood", c("star", "star-half-o", "star-o"), "star"
#'     ))
#'   }
#' )}
icon_state_button <- function(id, states, value = NULL, ...) {
  if (length(value) != 1) {
    state <- 1
  } else if (is.numeric(value)) {
    state <- value
  } else if (value %in% states) {
    state <- match(value, states)
  } else {
    state <- 1
  }
  value <- if (is.null(value)) 1 else value
  js_states <- jsonlite::toJSON(states, auto_unbox = TRUE)
  shiny::tags$button(
    class = "btn btn-default state-button", id = id, shiny::icon(states[state]),
    "data-state" = state, "data-states" = js_states, ...,
    htmltools::htmlDependency(
      "stateButton", "0.0.1", c(href = "dqshinyRes"),
      script = "js/stateButton-binding.js"
    )
  )
}

#' @description Changes the value or the options of an icon_state_button
#' on the client side.
#'
#' @param session the shiny session object
#'
#' @return update_icon_state_button: message to the client
#' @export
#' @rdname icon_state_button
update_icon_state_button <- function(session, id, states = NULL, value = NULL) {
  if (length(value) != 1) {
    state <- NULL
  } else if (is.numeric(value)) {
    state <- value
  } else if (value %in% states) {
    state <- match(value, states)
  } else {
    state <- NULL
  }
  message <- shiny:::dropNulls(list(states = states, state = state))
  session$sendInputMessage(id, message)
}
