dq_pop <- function(x, title, content, trigger = NULL, direction = NULL, options = NULL) {
  if (length(x) == 0L) return()
  arg_list <- not_null(list(
    tag = x, title = title, "data-trigger" = trigger, "data-toggle" = "popover",
    "data-placement" = direction, "data-content" = content
  ))
  x <- do.call(shiny::tagAppendAttributes, arg_list)
  id <- x$attribs$id
  if (is.null(id)) id <- paste0(sample(letters, 10), collapse = "")
  shiny::tagList(
    x, tags$script(paste0("$('#", id, "').popover(", jsonlite::toJSON(
      options, auto_unbox = TRUE
    ), ")"), if (isTRUE(trigger == "manual")) {
      paste0(
        "$('#", id, "').popover('show');",
        "$('#' + $('#", id, "').attr('aria-describedby')).addClass('hidden');",
        "$('#", id, "').click(function(e) {",
        "$('#' + $(this).attr('aria-describedby')).toggleClass('hidden');",
        "});"
        # use focus/focusout instead
      )
    })
  )
}
