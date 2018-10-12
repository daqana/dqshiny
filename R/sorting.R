#' @author richard.kunze
add_sorting_observer <- function(input, session, dq_values, context, paged, page_id) {
  dirs <- c("", "up", "down")
  sorts <- paste("sort", context, shiny::isolate(names(dq_values[[context]])), sep = "_")
  ignore <- structure(rep(TRUE, length(sorts)), names = sorts)
  lapply(seq(sorts), function(i) {
    s <- sorts[i]
    shiny::observeEvent(input[[s]], {
      if (ignore[s]) {
        ignore[s] <<- FALSE
      } else {
        dq_values$sort_col <- i
        dq_values$sort_dir <- dirs[input[[s]]]
        lapply(sorts[sorts != s], function(n) {
          if (length(input[[n]]) == 1L && input[[n]] != 1L) {
            ignore[n] <<- TRUE
            update_icon_state_button(session, n, value = 1L)
          }
        })
      }
    }, ignoreInit = TRUE)
  })

  shiny::observeEvent(list(dq_values$sort_col, dq_values$sort_dir), {
    dq_values[[context]] <- sort_data(dq_values[[context]], dq_values$sort_dir, dq_values$sort_col)
    if (paged) {
      size <- as.integer(input[[paste0("sel_", context, "_pageSize")]])
      num <- input[[paste0("num_", context, "_page")]]
      dq_values[[page_id]] <- update_page(dq_values[[context]], context, num, size, session)
    }
  }, ignoreInit = TRUE)
  sorts
}

#' @author richard.kunze
sort_data <- function(df, sort_dir, sort_col) {
  if (length(sort_dir) == 0 || sort_dir == "") {
    row_names <- suppressWarnings(as.numeric(rownames(df)))
    if (any(is.na(row_names))) df[order(rownames(df)), , drop = FALSE]
    else df[order(row_names), , drop = FALSE]
  } else {
    df[order(df[, sort_col], decreasing = (sort_dir == "down")), , drop = FALSE]
  }
}

#' @author richard.kunze
sort_button <- function(context, name, value = NULL) {
  id <- paste("sort", context, name, sep = "_")
  if (length(value) > 0L) value <- paste0("sort-", value)
  icon_state_button(id, c("sort", "sort-up", "sort-down"), value, class = "sort-button")
}
