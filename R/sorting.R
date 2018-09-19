#' @author richard.kunze
add_sorting_observer <- function(input, session, dq_values, context, paged, page_id) {
  dirs <- c("", "up", "down")
  sorts <- paste("sort", context, shiny::isolate(names(dq_values$full)), sep = "_")
  lapply(seq(sorts), function(i) {
    s <- sorts[i]
    shiny::observeEvent(input[[s]], {
      dq_values$sort_col <- i
      dq_values$sort_dir <- dirs[input[[s]]]
      lapply(sorts[sorts != s], function(n) update_icon_state_button(session, n, value = 1L))
    }, ignoreInit = TRUE)
  })

  shiny::observeEvent(list(dq_values$sort_col, dq_values$sort_dir), {
    dq_values[[context]] <- sort_data(dq_values[[context]], dq_values$sort_dir, dq_values$sort_col)
    if (paged) {
      pS <- as.integer(input[[paste0("sel_", context, "_pageSize")]])
      dq_values[[page_id]] <- update_page(dq_values[[context]], context, input[[paste0("num_", context, "_page")]], pS, session)
    }
  }, ignoreInit = TRUE)
  sorts
}

#' @author richard.kunze
sort_data <- function(df, sort_dir, sort_col) {
  if (length(sort_dir) == 0 || sort_dir == "") {
    df[order(as.numeric(rownames(df))), , drop = FALSE]
  } else {
    df[order(df[, sort_col], decreasing = (sort_dir == "down")), , drop = FALSE]
  }
}

#' @author richard.kunze
sort_button <- function(context, name, input = NULL) {
  id <- paste("sort", context, name, sep = "_")
  val <- if (is.null(input[[id]])) NULL else input[[id]]
  icon_state_button(id, c("sort", "sort-up", "sort-down"), val, class = "sort-button")
}
