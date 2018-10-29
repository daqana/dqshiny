#' @author richard.kunze
filter_row <- function(context, dq_values, filters = "T", reset = TRUE, sorting = NULL) {
  data <- shiny::isolate(dq_values[[context]])
  filters <- correct_filters(filters, length(data))
  to_sort <- length(sorting) > 0L && sorting != FALSE
  sort_dir <- sort_col <- ""
  if (to_sort && !is.logical(sorting) && !is.null(names(sorting))) {
    sort_dir <- sorting[["dir"]]
    sort_col <- sorting[["col"]]
  }
  class <- paste0("filter-row", if (to_sort) " sorting")
  res <- shiny::fluidRow(class = class)
  if (all(filters == "")) return(res)
  l <- lapply(seq(filters), function(i) {
    f <- filters[i]
    id <- paste("filter", context, names(data)[i], sep = "_")
    el <- shiny::div(class = "form-group")
    if (f == "T") {
      el <- shiny::textInput(id, NULL, placeholder = names(data)[i])
    } else if (f == "A") {
      choices <- c("", sort(unique(as.character(data[[i]]))))
      el <- autocomplete_input(id, NULL, choices, placeholder = names(data)[i])
    } else if (f == "S") {
      choices <- c("")
      names(choices) <- names(data)[i]
      choices <- c(choices, sort(unique(as.character(data[[i]]))))
      el <- shiny::selectizeInput(id, NULL, choices, options = list(dropdownParent = "body"))
    } else if (f %in% c("R", "D")) {
      tryCatch({
        d <- unlist(data[[i]])
        if (f == "R") {
          suppressWarnings({
            min_val <- min(d, na.rm = TRUE)
            max_val <- max(d, na.rm = TRUE)
            mi <- as.numeric(min_val)
            ma <- as.numeric(max_val)
          })
          if (!any(is.na(c(mi, ma)) | is.infinite(c(mi, ma))) ||
              !any(is.na(as.Date.character(c(min_val, max_val), "%Y-%m-%d")))) {
            el <- shiny::sliderInput(id, NULL, min_val, max_val, c(min_val, max_val))
          }
        } else if (f == "D") {
          min_d <- min(as.Date.character(d, "%Y-%m-%d"), na.rm = TRUE)
          max_d <- max(as.Date.character(d, "%Y-%m-%d"), na.rm = TRUE)
          el <- shiny::dateRangeInput(id, NULL, min_d, max_d, min_d, max_d)
        }
      }, error = function(e) print(e$message))
    }
    if (to_sort && f != "") {
      val <- NULL
      if (length(sort_col) > 0L && sort_col == names(data)[i]) {
        dq_values$sort_col <- i
        dq_values$sort_dir <- val <- sort_dir
      }
      el <- shiny::tagAppendChild(el, sort_button(context, names(data)[i], val))
    }
    el
  })
  if (reset) {
    res <- shiny::tagAppendChild(res, shiny::div(class = "reset-wrapper", shiny::actionButton(
      paste("reset", context, "filter", sep = "_"), "X", class = "dq-btn-sm", title = "Reset filters")))
  }
  if (length(l) > 0) res <- shiny::tagAppendChildren(res, l)
  res
}

#' @author richard.kunze
update_filters <- function(data, filters, context, session) {
  if (length(data) == 0L || length(filters) == 0L) return()
  filters <- correct_filters(filters, length(data))
  els <- grep(paste0("^filter_", context),
              shiny::isolate(names(session$input)), value = TRUE)
  names(els) <- gsub(paste0("^filter_", context, "_"), "", els)
  for (n in names(els)) {
    filter <- filters[names(data) == n]
    if (length(filter) == 1L) {
      if (filter == "S") {
        shiny::updateSelectInput(
          session, unname(els[n]), choices = sort(unique(data[[n]])),
          selected = shiny::isolate(session$input[[els[n]]])
        )
      } else if (filter == "R") {
        suppressWarnings({
          min_val <- min(data[[n]], na.rm = TRUE)
          max_val <- max(data[[n]], na.rm = TRUE)
        })
        shiny::updateSliderInput(session, unname(els[n]),
                                 min = min_val, max = max_val)
      }
    }
  }
}

#' @author richard.kunze
correct_filters <- function(filters, len) {
  if (length(filters) != len) filters <- rep_len(filters, len)
  filters <- toupper(substr(unlist(filters), 1L, 1L))
  filters[!filters %in% c("T", "S", "R", "A", "D", "")] <- "T"
  filters
}
