#' @author richard.kunze
#' @keywords internal
send_message <- function(type, ...) {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$sendCustomMessage(type = type, message = list(...))
  }
}
