#' @author richard.kunze
add_sorting_observer <- function(input, session, dqv, page_size, col_names) {
  dirs <- c("", "up", "down")
  sorts <- paste("sort", col_names, sep = shiny::ns.sep)
  ignore <- structure(rep(TRUE, length(sorts)), names = sorts)
  lapply(seq(sorts), function(i) {
    s <- sorts[i]
    shiny::observeEvent(input[[s]], {
      if (ignore[s]) {
        ignore[s] <<- FALSE
      } else {
        dqv$sorting <- list(col = i, dir = dirs[input[[s]]])
      }
    }, ignoreInit = TRUE)
  })

  shiny::observeEvent(dqv$sorting, {
    s <- sorts[dqv$sorting$col]
    if (length(s) == 1L && !is.na(s)) {
      lapply(sorts[sorts != s], function(n) {
        if (length(input[[n]]) == 1L && input[[n]] != 1L) {
          ignore[n] <<- TRUE
          update_icon_state_button(session, n, value = 1L)
        }
      })
    }
  }, ignoreInit = TRUE)
  sorts
}

#' @author richard.kunze
check_sorting <- function(sorting, to_sort, columns) {
  if (!isTRUE(to_sort)) return(NULL)
  if (length(sorting) != 2L || is.null(names(sorting)))
    return(list(dir = "", col = ""))
  dir <- sorting[["dir"]]
  if (!is.character(dir) || !(dir %in% c("", "up", "down"))) {
    sorting[["dir"]] <- ""
  }
  col <- sorting[["col"]]
  if (is.numeric(col) && col %in% seq(columns)) {
    sorting[["col"]] <- columns[[col]]
  } else if (!(is.character(col) && col %in% columns)) {
    sorting[["col"]] <- ""
  }
  as.list(sorting)
}

#' @author richard.kunze
sort_data <- function(df, sorting) {
  if (is.atomic(sorting) || length(sorting$dir) == 0L || sorting$dir == "") {
    row_names <- as_numeric(rownames(df))
    ord <- if (any(is.na(row_names))) order(rownames(df)) else order(row_names)
  } else {
    ord <- order(df[, sorting$col], decreasing = (sorting$dir == "down"))
  }
  df[ord, , drop = FALSE]
}

#' @author richard.kunze
sort_button <- function(ns, name, value = NULL) {
  id <- ns("sort", name)
  if (length(value) > 0L) value <- paste0("sort-", value)
  icon_state_button(id, c("sort", "sort-up", "sort-down"), value, class = "sort-button")
}
