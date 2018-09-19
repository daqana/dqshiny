#' @author richard.kunze
#' @keywords internal
send_message <- function(type, ids, ...) {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    for (id in ids) {
      session$sendCustomMessage(type = type, message = list(id = id, ...))
    }
  }
}
