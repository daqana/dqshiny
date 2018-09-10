#' Functions to create and use filter
#'
#' @description FilterRow creates a fluidRow with several textInputs or selectInputs for filtering a table.
#' Will also add a button to clear all filter values.
#'
#' @param context a string context to specify ui element id's
#' @param data data to show in the table, should be a data.frame
#' @param filters optional character vector, adds filters for each column, values must be one of c("T", "S", "R", "") to add a
#' TextFilter, SelectFilter, RangeFilter or none, vectors of length one will add a filter of this type for each column
#' @param reset optional, should a reset button be added or not
#'
#' @return filter_row: fluidRow containing the filters
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' library(rhandsontable)
#'
#' hw <- c("Hello", "my", "funny", "world!")
#' data <- data.frame(A=rep(hw, 5), B=rep(hw[c(2,3,4,1)], 5), C=1:20, D=Sys.Date()-0:19,
#'   stringsAsFactors = F)
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
#' )}
filter_row <- function(context, data, filters = "T", reset = TRUE) {
  filters <- correct_filters(filters, data)
  w <- 12 / sum(filters != "")
  fClass <- "filter-row"
  if (w != floor(w)) {
    fClass <- paste(fClass, "vertical-align")
  }
  res <- shiny::fluidRow(class = fClass)
  if (all(filters == "")) return(res)
  l <- lapply(seq(filters), function(i) {
    f <- filters[i]
    id <- paste("filter", context, names(data)[i], sep = "_")
    suppressWarnings({
      minV <- min(unlist(data[[i]]))
      maxV <- max(unlist(data[[i]]))
      mi <- as.numeric(minV)
      ma <- as.numeric(maxV)
    })
    inputEl <- NULL
    if (f == "R" && (!any(is.na(c(mi, ma)) | is.infinite(c(mi, ma))) || all(grepl("\\d{4}-\\d{2}-\\d{2}", c(minV, maxV))))) {
      inputEl <- shiny::sliderInput(id, NULL, minV, maxV, c(minV, maxV))
    } else if (f == "S") {
      choices <- c("")
      names(choices) <- names(data)[i]
      inputEl <- shiny::selectInput(id, NULL, c(choices, sort(unique(data[[i]]))))
    } else if (f != "") {
      inputEl <- shiny::textInput(id, NULL, placeholder = names(data)[i])
    }
    if (w == floor(w)) {
      shiny::column(w, inputEl)
    } else {
      inputEl
    }
  })
  if (reset) {
    res <- shiny::tagAppendChild(res, shiny::actionButton(
      paste("reset", context, "filter", sep = "_"), "X", class = "dq-btn-sm", title = "Reset filters"))
  }
  if (length(l) > 0) res <- shiny::tagAppendChildren(res, l)
  res
}

correct_filters <- function(filters, data) {
  if (length(filters) != length(data)) filters <- rep_len(filters, length(data))
  filters <- toupper(filters)
  filters[!filters %in% c("T", "S", "R", "")] <- "T"
  filters
}
