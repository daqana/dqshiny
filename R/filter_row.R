#' Functions to create and use filter
#'
#' @description FilterRow creates a fluidRow with several textInputs or
#' selectInputs for filtering a table.#' Will also add a button to clear
#' all filter values.
#'
#' @param context a string context to specify ui element id's
#' @param data data to show in the table, should be a data.frame
#' @param filters optional character vector, adds filters for each column,
#' values must be one of c("T", "S", "R", "") to add a TextFilter, SelectFilter,
#' RangeFilter or none, vectors of length one will add a filter of this type
#' for each column
#' @param reset optional, should a reset button be added or not
#' @param sorting optional, wheather to add buttons to sort data or not
#'
#' @return filter_row: fluidRow containing the filters
#' @author richard.kunze
#' @export
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(rhandsontable)
#' hw <- c("Hello", "my", "funny", "world!")
#' data <- data.frame(A=rep(hw, 5), B=rep(hw[c(2,3,4,1)], 5), C=1:20,
#'   D=Sys.Date()-0:19, stringsAsFactors = FALSE)
#' rVals <- reactiveValues()
#' rVals[["rand"]] <- data
#'
#' shinyApp(
#'   ui = fluidPage(
#'     filter_row("rand", data, c("T", "S", "R", "R"), FALSE),
#'     rHandsontableOutput("randomTable")
#'   ),
#'   server = function(input, output) {
#'     output$randomTable <- renderRHandsontable({
#'       rhandsontable(rVals[["rand"]])
#'     })
#'     observeEvent(get_filters(input, "rand"), {
#'       fVals <- get_filters(input, "rand")
#'       df <- text_filter(data, fVals[sapply(fVals, function(x) length(x) == 1)])
#'       rVals[["rand"]] <- range_filter(df, fVals[sapply(fVals, function(x) length(x) > 1)])
#'     }, ignoreInit = TRUE)
#'   }
#' )
#'
#' }
filter_row <- function(context, data, filters = "T", reset = TRUE, sorting = FALSE) {
  filters <- correct_filters(filters, length(data))
  class <- paste0("filter-row", if (sorting) " sorting")
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
    } else if (f == "R") {
      try({
        suppressWarnings({
          min_val <- min(unlist(data[[i]]))
          max_val <- max(unlist(data[[i]]))
          mi <- as.numeric(min_val)
          ma <- as.numeric(max_val)
        })
        if (!any(is.na(c(mi, ma)) | is.infinite(c(mi, ma))) || all(grepl("\\d{4}-\\d{2}-\\d{2}", c(min_val, max_val)))) {
          el <- shiny::sliderInput(id, NULL, min_val, max_val, c(min_val, max_val))
        }
      })
    }
    if (sorting && f != "") {
      el <- shiny::tagAppendChild(el, sort_button(context, names(data)[i]))
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
    if (length(filter) == 1) {
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
  filters <- toupper(substr(unlist(filters), 1, 1))
  filters[!filters %in% c("T", "S", "R", "A", "")] <- "T"
  filters
}
