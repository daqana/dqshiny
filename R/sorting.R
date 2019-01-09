#' @author richard.kunze
add_sorting_observer <- function(input, session, dqv, page_size, page_id) {
  dirs <- c("", "up", "down")
  sorts <- paste("sort", shiny::isolate(names(dqv$reduced)), sep = shiny::ns.sep)
  ignore <- structure(rep(TRUE, length(sorts)), names = sorts)
  lapply(seq(sorts), function(i) {
    s <- sorts[i]
    shiny::observeEvent(input[[s]], {
      if (ignore[s]) {
        ignore[s] <<- FALSE
      } else {
      dqv$sort_col <- i
      dqv$sort_dir <- dirs[input[[s]]]
      }
    }, ignoreInit = TRUE)
  })

  shiny::observeEvent(list(dqv$sort_col, dqv$sort_dir), {
    s <- sorts[dqv$sort_col]
    if (length(s) == 1L && !is.na(s)) {
      dqv$reduced <- sort_data(dqv$reduced, dqv$sort_dir, dqv$sort_col)
      lapply(sorts[sorts != s], function(n) {
        if (length(input[[n]]) == 1L && input[[n]] != 1L) {
          ignore[n] <<- TRUE
          update_icon_state_button(session, n, value = 1L)
        }
      })
    }
    if (length(page_size) > 0L && page_size > 0L) {
      size <- as.integer(input$pageSize)
      if (length(size) == 0L) size <- page_size
      num <- input$pageNum
      dqv[[page_id]] <- update_page(dqv$reduced, num, size, session)
    }
  })
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
sort_button <- function(ns, name, value = NULL) {
  id <- ns("sort", name)
  if (length(value) > 0L) value <- paste0("sort-", value)
  icon_state_button(id, c("sort", "sort-up", "sort-down"), value, class = "sort-button")
}
