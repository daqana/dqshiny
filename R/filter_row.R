#' @author richard.kunze
filter_row <- function(ns, dqv, filters, columns, sorting, reset = TRUE) {
  data <- shiny::isolate(dqv$full[, columns, drop = FALSE])
  to_sort <- length(sorting) > 0L
  class <- paste0("filter-row", if (to_sort) " sorting")
  res <- shiny::fluidRow(class = class)
  if (is.null(filters)) return(res)
  l <- lapply(seq(filters), function(i) {
    d <- unlist(data[[i]])
    n <- names(data)[i]
    f <- filters[i]
    id <- ns("filter", n)
    el <- shiny::div(class = "form-group")
    if (f == "T") {
      el <- shiny::textInput(id, NULL, placeholder = n)
    } else if (f == "A") {
      choices <- c("", sort(unique(as.character(d))))
      el <- autocomplete_input(id, NULL, choices, placeholder = n)
    } else if (f == "S") {
      choices <- c("")
      names(choices) <- n
      choices <- c(choices, sort(unique(as.character(d))))
      el <- shiny::selectizeInput(id, NULL, choices, options = list(dropdownParent = "body"))
    } else if (f == "R") {
      suppressWarnings({
        min_val <- min(d, na.rm = TRUE)
        max_val <- max(d, na.rm = TRUE)
      })
      el <- shiny::sliderInput(id, NULL, min_val, max_val, c(min_val, max_val))
    } else if (f == "D") {
      suppressWarnings({
        min_d <- min(as.Date.character(d, "%Y-%m-%d"), na.rm = TRUE)
        max_d <- max(as.Date.character(d, "%Y-%m-%d"), na.rm = TRUE)
      })
      el <- shiny::dateRangeInput(id, NULL, min_d, max_d, min_d, max_d)
    }

    if (to_sort && f != "") {
      val <- NULL
      if (length(sorting$col) == 1L && sorting$col == n) {
        dqv$sorting <- list(col = i, dir = sorting$dir)
        val <- sorting$dir
      }
      el <- shiny::tagAppendChild(el, sort_button(ns, n, val))
    }
    el
  })
  if (reset) {
    res <- shiny::tagAppendChild(res, shiny::div(class = "reset-wrapper", shiny::actionButton(
      ns("filter-reset"), "X", class = "dq-btn-sm", title = "Reset filters")))
  }
  if (length(l) > 0) res <- shiny::tagAppendChildren(res, l)
  res
}

#' @author richard.kunze
update_filters <- function(data, filters, session) {
  if (length(data) == 0L || length(filters) == 0L) return()
  els <- paste0("filter-", names(data))
  for (i in seq(els)) {
    filter <- filters[i]
    if (length(filter) == 1L) {
      if (filter == "S") {
        ch <- c()
        ch[names(data)[i]] <- ""
        shiny::updateSelectInput(
          session, els[i], choices = c(ch, sort(unique(data[[i]])))
        )
      } else if (filter == "R") {
        suppressWarnings({
          min_val <- min(data[[i]], na.rm = TRUE)
          max_val <- max(data[[i]], na.rm = TRUE)
        })
        shiny::updateSliderInput(
          session, els[i], min = min_val, max = max_val
        )
      } else if (filter == "D") {
        suppressWarnings({
          d <- as.Date.character(data[[i]], "%Y-%m-%d")
          min_val <- min(d, na.rm = TRUE)
          max_val <- max(d, na.rm = TRUE)
        })
        shiny::updateDateRangeInput(
          session, els[i], min = min_val, max = max_val
        )
      }
    }
  }
}

#' @author richard.kunze
correct_filters <- function(vals, data) {
  if (isTRUE(all(vals == ""))) return(NULL)
  len <- length(data)
  if (length(vals) != len) vals <- rep_len(vals, len)
  vals <- toupper(substr(unlist(vals), 1L, 1L))
  vals[!vals %in% c("T", "S", "R", "A", "D", "", NA)] <- NA
  vapply(seq(vals), function(i) correct_type(vals[i], data[[i]]), "")
}

#' @author richard.kunze
correct_type <- function(type, vec) {
  if (length(type) != 1L || type %in% c("T", "A", "S", "")) return(type)
  suppressWarnings({
    log_vec <- as.logical(vec)
    num_vec <- as.numeric(vec)
    date_vec <- as.Date.character(vec, "%Y-%m-%d")
  })
  if (type %in% c("D", NA) && !all(is.na(date_vec))) return("D")
  if (!all(is.na(num_vec)) || !all(is.na(date_vec))) return("R")
  if (length(vec) > 0L && length(unique(vec)) <= sqrt(length(vec))) return("S")
  return("T")
}
