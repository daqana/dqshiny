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
    ft <- filters[[i]]$type
    fv <- filters[[i]]$value
    id <- ns("filter", n)
    el <- shiny::div(class = "form-group")
    if (ft == "T") {
      el <- shiny::textInput(id, NULL, value = fv, placeholder = n)
    } else if (ft == "A") {
      choices <- c("", sort(unique(as.character(d))))
      el <- autocomplete_input(id, NULL, choices, value = fv, placeholder = n)
    } else if (ft == "S") {
      choices <- c("")
      names(choices) <- n
      d <- c(d, if (!is.null(fv)) fv)
      choices <- c(choices, sort(unique(as.character(d))))
      el <- shiny::selectizeInput(id, NULL, choices, selected = fv, options = list(dropdownParent = "body"))
    } else if (ft == "R") {
      suppressWarnings({
        min_v <- min(as.numeric(d), na.rm = TRUE)
        max_v <- max(as.numeric(d), na.rm = TRUE)
      })
      if (is.null(fv)) {
        fv[[1]] <- min_v
        fv[[2]] <- max_v
      }
      el <- shiny::sliderInput(
        id, NULL, min(min_v, fv[[1]]), max(max_v, fv[[1]]), c(fv[[1]], fv[[2]])
      )
    } else if (ft == "D") {
      suppressWarnings({
        min_d <- min(as.Date.character(d, "%Y-%m-%d"), na.rm = TRUE)
        max_d <- max(as.Date.character(d, "%Y-%m-%d"), na.rm = TRUE)
      })
      if (is.null(fv)) {
        fv[[1]] <- min_d
        fv[[2]] <- max_d
      }
      el <- shiny::dateRangeInput(
        id, NULL, fv[[1]], fv[[2]], min(min_d, fv[[1]]), max(max_d, fv[[2]])
      )
    }

    if (to_sort && ft != "") {
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
    filter <- filters[[i]]$type
    if (length(filter) == 1L && !is.na(filter)) {
      if (filter == "S") {
        ch <- c()
        ch[names(data)[i]] <- ""
        shiny::updateSelectInput(
          session, els[i], choices = c(ch, sort(unique(data[[i]])))
        )
      } else if (filter == "R") {
        suppressWarnings({
          min_val <- min(as.numeric(data[[i]]), na.rm = TRUE)
          max_val <- max(as.numeric(data[[i]]), na.rm = TRUE)
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
  if (length(vals) == 1L) vals <- rep(vals, len)
  if (length(vals) != len) vals <- rep(NA, len)
  lapply(seq(vals), function(i) {
    v <- vals[[i]]
    value <- NULL
    type <- NA_character_
    if (length(v) == 1L || is.null(names(v))) {
      type <- v[1]
    } else {
      if (!is.null(v[["value"]])) value <- v[["value"]]
      if (!is.null(v[["type"]])) type <- v[["type"]]
    }
    # TODO add correct_value(value, type)
    list(type = correct_type(type, data[[i]]), value = value)
  })
}

#' @author richard.kunze
correct_type <- function(type, vec) {
  if (length(type) != 1L) return("")
  type <- toupper(substr(type, 1L, 1L))
  if (!(type %in% c("T", "S", "R", "A", "D", "", NA))) type <- NA
  if (type %in% c("T", "A", "S", "")) return(type)
  if (type %in% c("S", NA) && is.factor(vec)) return("S")
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
