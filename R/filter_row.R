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
    el <- switch(ft,
      "T" = shiny::textInput(id, NULL, value = fv, placeholder = n),
      "A" = auto_input(id, d, fv, n),
      "S" = select_input(id, d, fv, n),
      "R" = range_input(id, as_numeric(d), fv, ft),
      "D" = range_input(id, as_date(d), fv, ft),
      shiny::div(class = "form-group")
    )
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
    res <- shiny::tagAppendChild(
      res, shiny::div(class = "reset-wrapper", shiny::actionButton(
        ns("filter-reset"), "X", class = "dq-btn-sm", title = "Reset filters"
      ))
    )
  }
  if (length(l) > 0) res <- shiny::tagAppendChildren(res, l)
  res
}

#' @author richard.kunze
auto_input <- function(id, d, fv, n) {
  choices <- c("", sort(unique(as.character(c(d, fv)))))
  autocomplete_input(id, NULL, choices, value = fv, placeholder = n)
}

#' @author richard.kunze
select_input <- function(id, d, fv, n) {
  choices <- c("")
  names(choices) <- n
  d <- c(d, fv)
  choices <- c(choices, sort(unique(as.character(d))))
  shiny::selectizeInput(
    id, NULL, choices, selected = fv, options = list(dropdownParent = "body")
  )
}

#' @author richard.kunze
range_input <- function(id, d, fv, ft) {
  min_v <- min(d, na.rm = TRUE)
  max_v <- max(d, na.rm = TRUE)
  if (length(fv) != 2L) fv <- c(min_v, max_v)
  min_v <- min(min_v, fv[[1]])
  max_v <- max(max_v, fv[[2]])
  if (ft == "R") {
    shiny::sliderInput(id, NULL, min_v, max_v, c(fv[[1]], fv[[2]]))
  } else {
    shiny::dateRangeInput(id, NULL, fv[[1]], fv[[2]], min_v, max_v)
  }
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
        ch <- c(ch, sort(unique(data[[i]])))
        shiny::updateSelectInput(session, els[i], choices = ch)
      } else if (filter == "R") {
        d <- as_numeric(data[[i]])
        min_v <- min(d, na.rm = TRUE)
        max_v <- max(d, na.rm = TRUE)
        shiny::updateSliderInput(session, els[i], min = min_v, max = max_v)
      } else if (filter == "D") {
        d <- as_date(data[[i]])
        min_v <- min(d, na.rm = TRUE)
        max_v <- max(d, na.rm = TRUE)
        shiny::updateDateRangeInput(session, els[i], min = min_v, max = max_v)
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
    type <- correct_type(type, data[[i]])
    list(type = type, value = correct_value(value, type))
  })
}

#' @author richard.kunze
correct_type <- function(type, vec) {
  if (length(type) != 1L) return("")
  type <- toupper(substr(type, 1L, 1L))
  if (!(type %in% c("T", "S", "R", "A", "D", "", NA))) type <- NA
  if (type %in% c("T", "A", "S", "")) return(type)
  if (type %in% c("S", NA) && is.factor(vec)) return("S")
  num_vec <- as_numeric(vec)
  date_vec <- as_date(vec)
  if (type %in% c("D", NA) && !all(is.na(date_vec))) return("D")
  if (!all(is.na(num_vec)) || !all(is.na(date_vec))) return("R")
  if (length(vec) > 0L && length(unique(vec)) <= sqrt(length(vec))) return("S")
  "T"
}

#' @author richard.kunze
correct_value <- function(value, type) {
  if (length(value) == 0L) return()
  if (length(type) != 1L || type == "") return()
  if (type %in% c("T", "A", "S")) return(as.character(value))
  if (length(value) != 2L) return()
  if (type == "R") {
    value <- as_numeric(value)
    if (any(is.na(value))) return()
    else return(value)
  }
  if (type == "R") {
    value <- as_date(value)
    if (any(is.na(value))) return()
    else return(value)
  }
  NULL
}
