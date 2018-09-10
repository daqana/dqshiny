#' Adds an uiOutput
#'
#' @description Adds a fluidrow containing a column with the given width, ready to support a dq_handsontable_output
#' (or any other rendered ui).
#'
#' @param id id of the element
#' @param width width of the table in bootstrap columns
#' @param offset optional offset of the column
#'
#' @return fluidrow containing a column with the output fields
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' library(rhandsontable)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     dq_handsontable_output("myCars")
#'   ),
#'   server = function(input, output) {
#'     dq_render_handsontable("myCars", mtcars, "cars", filters = "R",
#'       paged = FALSE, reset = FALSE)
#'   }
#' )}
dq_handsontable_output <- function(id, width = 12, offset = 0) {
  requireNamespace("rhandsontable")
  requireNamespace("shiny")
  if (is.null(id)) return(NULL)
  shiny::fluidRow(shiny::column(
    width, offset = offset, shiny::uiOutput(paste0(id, "_filters")),
    rhandsontable::rHandsontableOutput(id), shiny::uiOutput(paste0(id, "_pages"))
  ))
}

#' Renders a dq handsontable
#'
#' @description Renders a handsontable into the given uiOutput id with the given data and parameters.
#' Can also contain several filters to filter the data and a feature to split the table into several pages with a given page size.
#' The function will also add all needed observeEvents to establish the required functionalities. If table is not readOnly, all
#' user inputs will automatically stored  and updated independent from any filters or pages.
#'
#' @param id id of the handsontable element, should be the same as the one used in the output element
#' @param data data to show in the table, should be a data.frame
#' @param context the context used to specify all ui elements used for this table
#' @param filters optional character vector, adds filters for each column, values must be one of c("T", "S", "R", "") to add a
#' TextFilter, SelectFilter, RangeFilter or none, vectors of length one will add a filter of this type for each column, set to
#' NULL to ommit filter row
#' @param paged optional logical, splits table into pages if TRUE
#' @param page_size optional numeric, number of items per page, can be one of 10, 25, 50, 100 or any other value
#' which will be added to this list
#' @param reset optional logical, specify whether to add a button to reset filters to initial values or not
#' @param table_params optional list, specify parameters to hand to rHandsontable table element
#' @param col_params optional list, specify parameters to hand to rHandsontable cols elements
#' @param ... further optional lists to specify parameters to hand to rHandsontable col elements,
#' those lists have to be assigned to a name
#'
#' @return reactive values of the data in the table
#' @author richard.kunze
#' @seealso \code{\link[rhandsontable:rhandsontable]{rhandsontable}}, \code{\link[rhandsontable:hot_cols]{hot_cols}} and \code{\link[rhandsontable:hot_col]{hot_col}}
#'
#' @examples \donttest{library(shiny)
#' library(rhandsontable)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     dq_handsontable_output("randomTable", 9)
#'   ),
#'   server = function(input, output, session) {
#'     hw <- c("Hello", "my", "funny", "world!")
#'     data <- data.frame(A=rep(hw, 500), B=rep(hw[c(2,3,4,1)], 500),
#'                        C=1:500, D=Sys.Date()-0:499, stringsAsFactors = F)
#'     dq_render_handsontable("randomTable", data, "rand", filters = c("S", "T", "R", "R"),
#'       pCol1=list(col=1, type = "dropdown", source = letters),
#'       pCol2=list(col=2:4, type = "dropdown", source = LETTERS))
#'   }
#' )}
dq_render_handsontable <- function(id, data, context, filters = "T", paged = TRUE, page_size = 25, reset = TRUE,
                                   table_params = list(readOnly = FALSE, stretchH = "all", contextMenu = FALSE),
                                   col_params = list(colWidths = 1, highlightCol = TRUE, highlightRow = TRUE, manualColumnResize = TRUE), ...) {
  requireNamespace("rhandsontable")
  requireNamespace("shiny")
  args <- list(...)
  if (!is.null(args[["session"]]) && inherits(args[["session"]], "ShinySession")) {
    session <- args[["session"]]
    args <- args[names(args) != "session"]
  } else {
    session <- shiny::getDefaultReactiveDomain()
  }
  if (is.null(id) || is.null(data) || !inherits(data, "data.frame") ||
      is.null(context) || is.null(session)) return()
  input <- session$input
  output <- session$output

  dq_values <- shiny::reactiveValues()
  dq_values[[context]] <- data
  dq_values[["full"]] <- data
  if (paged) {
    page_id <- paste0(context, "Page")
    page_select <- paste0("select_", context, "_page_size")
    dq_values[[page_id]] <- data[1:page_size, ]
    shiny::observeEvent(input[[paste0("num_", context, "_page")]], {
      pS <- as.integer(input[[page_select]])
      dq_values[[page_id]] <- update_page(dq_values[[context]], context, input[[paste0("num_", context, "_page")]], pS, session)
    }, ignoreInit = TRUE)
  } else {
    page_id <- context
  }

  if (!is.null(filters)) {
    output[[paste0(id, "_filters")]] <- shiny::renderUI({
      filter_row(context, data, filters, reset)
    })
  }

  params <- append(list(table_params, col_params), args)
  output[[id]] <- rhandsontable::renderRHandsontable({
    params[[1]][["data"]] <- dq_values[[page_id]]
    params[[2]][["hot"]] <- do.call(rhandsontable::rhandsontable, params[[1]])
    hot <- do.call(rhandsontable::hot_cols, params[[2]])
    if (length(params) > 2) {
      for (i in 3:length(params)) {
        if (!is.null(params[[i]])) {
          params[[i]][["hot"]] <- hot
          hot <- do.call(rhandsontable::hot_col, params[[i]])
        }
      }
    }
    hot
  })

  if (paged) {
    page_sizes <- sort(unique(c(page_size, 10, 25, 50, 100)))
    output[[paste0(id, "_pages")]] <- shiny::renderUI({
      shiny::fluidRow(
        class = "vertical-align", column(5, align = "right", style = "padding-right:0;", "Page:"),
        shiny::column(1, style = "padding-top:15px;", shiny::numericInput(paste0("num_", context, "_page"), NULL, 1, 1)),
        shiny::column(2, style = "padding-left:0;", shiny::textOutput(paste0(context, "_maxPages"), inline = TRUE)),
        shiny::column(3, style = "padding-right:0;", align = "right", "Items per page:"),
        shiny::column(1, style = "padding-top:15px;margin-bottom:-6px;", shiny::selectInput(page_select, NULL, page_sizes, page_size)))
    })
    output[[paste0(context, "_maxPages")]] <- shiny::renderText({
      sel <- as.integer(input[[page_select]])
      paste("of ", ceiling(max(length(dq_values[[context]][[1]]) / sel, 1)))
    })
    shiny::observeEvent(input[[page_select]], {
      sel <- as.integer(input[[page_select]])
      dq_values[[page_id]] <- update_page(dq_values[[context]], context, input[[paste0("num_", context, "_page")]], sel, session)
    })
  }

  if (reset) {
    shiny::observeEvent(input[[paste0("reset_", context, "_filter")]], {
      for (n in grep(paste0("^filter_", context), names(input), value = TRUE)) {
        shiny::updateTextInput(session, n, value = "")
        reset_slider_input(n)
      }
    })
  }

  shiny::observeEvent(get_filters(input, context), {
    f_vals <- get_filters(input, context)
    if (length(f_vals) == 0) return()
    df <- text_filter(dq_values[["full"]], f_vals[sapply(f_vals, function(x) length(x) == 1)])
    dq_values[[context]] <- range_filter(df, f_vals[sapply(f_vals, function(x) length(x) > 1)])
    if (paged) {
      sel <- as.integer(input[[page_select]])
      dq_values[[page_id]] <- update_page(dq_values[[context]], context, input[[paste0("num_", context, "_page")]], sel, session)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[id]], {
    if (!is.null(input[[id]]$changes$source)) {
      lapply(input[[id]]$changes$changes, function(c) {
        row <- c[[1]] + 1
        rowName <- as.character(rownames(rhandsontable::hot_to_r(input[[id]]))[row])
        col <- c[[2]] + 1
        val <- c[[4]]
        dq_values[["full"]][rowName, col] <- val
        dq_values[[context]][rowName, col] <- val
      })
      if (!is.null(filters)) {
        update_filters(dq_values[["full"]], filters, context, session)
      }
    }
  }, ignoreInit = TRUE)

  shiny::isolate(dq_values[["full"]])
}

#' @author richard.kunze
update_page <- function(df, context, page, size, session) {
  if (is.null(session) || is.null(df)) return()
  l <- nrow(df)
  if (is.null(page) || is.na(page)) page <- 1
  else if (is.infinite(page)) page <- l
  if (is.null(size) || is.na(size)) size <- 25
  else if (is.infinite(size)) size <- l

  maxP <- max(ceiling(l / size), 1)
  p <- ifelse(is.na(as.numeric(page)), 1, page)
  p <- max(min(p, maxP), 1)
  shiny::updateNumericInput(session, paste0("num_", context, "_page"), value = p, max = maxP)
  df[min(l, ((p - 1) * size + 1)):min(l, p * size), ]
}

#' @author richard.kunze
update_filters <- function(data, filters, context, session) {
  filters <- correct_filters(filters, data)
  els <- grep(paste0("^filter_", context), names(session$input), value = TRUE)
  names(els) <- gsub(paste0("^filter_", context, "_"), "", els)
  for (n in names(els)) {
    filter <- filters[which(names(data) == n)]
    if (length(filter) == 1) {
      if (filter == "S") {
        shiny::updateSelectInput(session, unname(els[n]), choices = sort(unique(data[[n]])), selected = session$input[[els[n]]])
      } else if (filter == "R") {
        minVal <- min(data[[n]], na.rm = TRUE)
        maxVal <- max(data[[n]], na.rm = TRUE)
        shiny::updateSliderInput(session, unname(els[n]), min = minVal, max = maxVal)
      }
    }
  }
}
