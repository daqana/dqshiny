#' Adds an uiOutput and renders an enhanced rhandsontable html widget
#'
#' @description dq_handsontable_output adds a fluidRow containing a column with
#' the given width, ready to support a dq_handsontable.
#'
#' @param id id of the element
#' @param width width of the table in bootstrap columns
#' @param offset optional offset of the column
#'
#' @return dq_handsontable_output: fluidRow containing the output fields
#' @rdname dq_render_handsontable
#' @export
dq_handsontable_output <- function(id, width = 12L, offset = 0L) {
  requireNamespace("rhandsontable")
  requireNamespace("shiny")
  if (is.null(id)) return(NULL)
  ns <- dq_NS(id)
  shiny::fluidRow(shiny::column(
    width, offset = offset,
    shiny::uiOutput(ns("filters")),
    rhandsontable::rHandsontableOutput(id),
    shiny::uiOutput(ns("pages")),
    init()
  ))
}

#' Adds an uiOutput and renders an enhanced rhandsontable html widget
#'
#' @description dq_render_handsontable renders a rhandsontable into the given
#' uiOutput id with the given data and parameters. Can also contain several
#' filters to filter the data and a feature to split the table into several
#' pages with a given page size. The function will also add all needed
#' observeEvents to establish the required functionalities. If table is not
#' readOnly, all user inputs will automatically stored and updated independent
#' from any filters, sortings or pages.
#'
#' @param data data to show in the table, should be a data.frame'ish object, can
#' also be reactive(Val) or a reactiveValues object holding the data under the
#' given id (e.g. myReactiveValues[[id]] <- data). In case of reactiveVal(ues)
#' data will always be in sync with user inputs.
#' @param context the context used to specify all ui elements used for this
#' table, can be omitted which ends up in a randomly generated context
#' NOTE: this parameter is deprecated and will be removed soon
#' @param filters optional character vector, adds filters for each column,
#' values must be one of "Text", "Select", "Range", "Date", "Auto" or "" (can be
#' abbreviated) to add a Text-, Select-, Range-, DateRange-, AutocompleteInput
#' or none, vectors of length one will add a filter of this type for each column
#' and NA will try to guess proper filters
#' @param reset optional logical, specify whether to add a button to reset
#' filters and sort buttons to initial values or not
#' @param page_size optional integer, number of items per page, can be one of
#' 10, 25, 50, 100 or any other value(s) which will be added to this list, first
#' value will be used initially, NULL will disable paging at all
#' @param sorting optional, specify whether to add sort buttons for every column
#' or not, as normal rhandsontable sorting won't work properly when table is
#' paged, value can be logical of length one or a vector specifying the initial
#' sort "col"umn and "dir"ection e.g. c(dir="down", col="Colname")
#' @param columns optional, specify which columns to show in the table, useful
#' in combination with reactive values, which will still hold all the data
#' @param width_align optional boolean to align filter widths with hot columns,
#' should only be used with either horizontal_scroll, stretchH = "all" or a
#' table fitting in its output element
#' @param horizontal_scroll optional boolean to scroll the filter row according
#' to the hot table, especially useful for tables with many columns
#' @param table_param optional list, specify parameters to hand to rhandsontable
#' table element
#' @param cols_param optional list, specify parameters to hand to rhandsontable
#' cols elements
#' @param col_param optional list of lists to specify parameters to hand to
#' rhandsontable col elements
#' @param cell_param optional list of lists to specify parameters to hand to
#' rhandsontable cells
#' @param session shiny session object
#'
#' @return dq_render_handsontable: the given data
#' @author richard.kunze
#' @export
#' @seealso \code{\link[rhandsontable:rhandsontable]{rhandsontable}},
#' \code{\link[rhandsontable:hot_cols]{hot_cols}} and
#' \code{\link[rhandsontable:hot_col]{hot_col}}
#'
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     dq_handsontable_output("randomTable", 9L)
#'   ),
#'   server = function(input, output, session) {
#'     hw <- c("Hello", "my", "funny", "world!")
#'     data <- data.frame(A = rep(hw, 500), B = hw[c(2,3,4,1)],
#'       C = 1:500, D = Sys.Date() - 0:499, stringsAsFactors = FALSE)
#'     dq_render_handsontable("randomTable", data,
#'       filters = c("A", NA, NA, NA), sorting = c(dir = "up", col = "B"),
#'       page_size = c(17L, 5L, 500L, 1000L), width_align = TRUE,
#'       col_param = list(list(col = 1L, type = "dropdown", source = letters)),
#'       cell_param = list(list(row = 2:9, col = 1:2, readOnly = TRUE))
#'     )
#'   }
#' )
#'
#' }
dq_render_handsontable <- function(
  id, data, context = NULL, filters = "T", page_size = 25L, reset = TRUE,
  sorting = NULL, columns = NULL, width_align = FALSE, horizontal_scroll = FALSE,
  table_param = NULL, cols_param = NULL, col_param = NULL, cell_param = NULL,
  session = shiny::getDefaultReactiveDomain()
) {
  requireNamespace("rhandsontable", quietly = TRUE)
  requireNamespace("shiny", quietly = TRUE)

  # initial settings
  if (is.null(id) || is.null(data) || is.null(session)) return()
  if (!missing(context)) {
    warning("Context parameter is deprecated and will be removed soon!")
  }
  if (length(columns) == 0L) columns <- TRUE

  ns <- dq_NS(id)
  app_input <- session$input
  app_output <- session$output

  session <- session$makeScope(id)
  input <- session$input
  output <- session$output

  table_data <- data

  dqv <- shiny::reactiveValues()

  paged <- length(page_size) > 0L && any(page_size > 0L)
  page_id <- "reduced"
  to_sort <- (length(sorting) > 0L && !identical(sorting, FALSE))
  no_update <- FALSE

  # used functions
  update_page_if_necessary <- function() {
    if (paged) {
      sel <- as.integer(input$pageSize)
      dqv[[page_id]] <- update_page(dqv$reduced, input$pageNum, sel, session)
    }
  }

  set_data <- function(df) {
    df <- as.data.frame(df)
    dqv$full <- dqv$reduced <- df
    if (length(df) > 0L) dqv$reduced <- df[, columns, drop = FALSE]
  }

  if (shiny::is.reactivevalues(table_data)) {
    shiny::observeEvent(table_data[[id]], {
      if (no_update) {
        no_update <<- FALSE
      } else {
        set_data(table_data[[id]])
        update_page_if_necessary()
        if (!is.null(filters)) {
          update_filters(dqv$full[, columns, drop = FALSE], filters, session)
        }
      }
    }, ignoreInit = TRUE)
    set_data(shiny::isolate(table_data[[id]]))
  } else if (shiny::is.reactive(table_data)) {
    shiny::observeEvent(table_data(), {
      if (no_update) {
        no_update <<- FALSE
      } else {
        set_data(table_data())
        update_page_if_necessary()
        if (!is.null(filters)) {
          update_filters(dqv$full[, columns, drop = FALSE], filters, session)
        }
      }
    }, ignoreInit = TRUE)
    set_data(shiny::isolate(table_data()))
  } else {
    set_data(table_data)
  }

  # define page_id which is needed for table rendering and reduce data to first page
  sorting <- check_sorting(sorting, to_sort, shiny::isolate(names(dqv$full)))
  if (paged) {
    page_id <- "page"
    df <- shiny::isolate(dqv$reduced)
    if (to_sort) df <- sort_data(df, sorting)
    n <- min(page_size[1L], nrow(df))
    dqv[[page_id]] <- df[1:n,]
  }

  # fill filter values and detect proper values for NA
  filters <- correct_filters(filters, shiny::isolate(dqv$full[, columns, drop = FALSE]))

  # render filter row and add observer for filters
  if (!is.null(filters)) {
    output$filters <- shiny::renderUI({
      filter_row(ns, dqv, filters, columns, sorting, reset)
    })
    shiny::observeEvent(get_filters(input), {
      f_vals <- get_filters(input)
      if (length(f_vals) == 0) return()
      df <- text_filter(dqv$full[, columns, drop = FALSE], f_vals[sapply(f_vals, function(x) length(x) == 1L)])
      dqv$reduced <- range_filter(df, f_vals[sapply(f_vals, function(x) length(x) == 2L)])
      if (to_sort) {
        dqv$reduced <- sort_data(dqv$reduced, dqv$sorting)
      }
      update_page_if_necessary()
    }, ignoreInit = TRUE)
  }

  # merge default table/cols parameters with given ones
  table_default <- list(readOnly = FALSE, stretchH = "all", contextMenu = FALSE)
  table_default <- append(table_param, table_default)
  table_default <- table_default[!duplicated(names(table_default))]

  cols_default <- list(colWidths = 1L, highlightCol = TRUE, dateFormat = "YYYY-MM-DD",
                       highlightRow = TRUE, manualColumnResize = TRUE)
  cols_default <- append(cols_param, cols_default)
  cols_default <- cols_default[!duplicated(names(cols_default))]

  params <- list(table_default, cols_default, col_param, cell_param)
  params[[1L]] <- add_scripts(params[[1L]], isTRUE(width_align),
                              isTRUE(horizontal_scroll))

  # render dq_handsontable
  app_output[[id]] <- rhandsontable::renderRHandsontable({
    params[[1L]]$data <- dqv[[page_id]]
    params[[2L]]$hot <- do.call(rhandsontable::rhandsontable, params[[1L]])
    hot <- do.call(rhandsontable::hot_cols, params[[2L]])
    for (x in params[[3L]]) {
      hot <- do.call(rhandsontable::hot_col, append(list(hot), x))
    }
    for (x in params[[4L]]) {
      x$row <- match(x$row, rownames(dqv[[page_id]]))
      x$row <- x$row[!is.na(x$row)]
      hot <- do.call(dq_hot_cell, append(list(hot), x))
    }
    hot$dependencies <- append(hot$dependencies, init())
    hot
  })

  # render paging row and add observer for inputs
  if (paged) {
    page_sizes <- sort(unique(c(page_size, 10L, 25L, 50L, 100L)))
    output$pages <- shiny::renderUI({
      paging_row(ns, page_size[1L], page_sizes)
    })
    output$maxPages <- shiny::renderText({
      sel <- as.integer(input$pageSize)
      paste("of ", ceiling(max(length(dqv$reduced[[1L]]) / sel, 1L)))
    })
    shiny::observeEvent(c(input$pageNum, input$pageSize), {
      if (!is.na(input$pageSize)) update_page_if_necessary()
    })
  }

  # add sort buttons
  if (to_sort) {
    sorts <- add_sorting_observer(input, session, dqv, page_size, page_id)
  }

  # add reset button
  if (reset) {
    shiny::observeEvent(input[["filter-reset"]], {
      for (n in grep("^filter", names(input), value = TRUE)) {
        shiny::updateTextInput(session, n, value = "")
        reset_slider_input(n)
      }
      if (to_sort) {
        dqv$sorting <- list(dir = "", col = "")
        lapply(sorts, function(n) update_icon_state_button(session, n, value = 1L))
      }
    })
  }

  # add observer for table changes
  shiny::observeEvent(app_input[[id]], {
    if (!is.null(app_input[[id]]$changes$source)) {
      row_names <- as.character(rownames(rhandsontable::hot_to_r(app_input[[id]])))
      col_names <- colnames(dqv$reduced)
      lapply(app_input[[id]]$changes$changes, function(ch) {
        row <- ch[[1L]] + 1L
        col <- ch[[2L]] + 1L
        dqv$reduced[row_names[row], col] <- ch[[4L]]
        dqv$full[row_names[row], col_names[col]] <- ch[[4L]]
      })
      if (shiny::is.reactivevalues(table_data)) {
        no_update <<- TRUE
        table_data[[id]] <- dqv$full
      } else if (inherits(table_data, "reactiveVal")) {
        no_update <<- TRUE
        table_data(dqv$full)
      }
      if (!is.null(filters)) {
        update_filters(dqv$full[, columns, drop = FALSE], filters, session)
      }
    }
  }, ignoreInit = TRUE)

  shiny::isolate(dqv$full)
}

#' @author richard.kunze
add_scripts <- function(params, width, scroll) {
  if (width || scroll) {
    params$afterRender <- htmlwidgets::JS(
      "function() {
        var hider = $(this.rootElement).find('.wtHider');
        var $filter = $(document.getElementById(this.rootElement.id + '-filters'));
        var that = this;
        $filter.ready(function() {
          $filter.css('overflow', 'hidden');
          var row = $filter.find('.row');
          row.width(hider.width());",
      if (width)
        "var els = $filter.find('.form-group');
         for (var i = 0; i < els.length; i++) {
           $(els[i]).outerWidth($(that.getCell(0, i)).outerWidth());
         }
       });",
      "}"
    )
  }
  if (scroll) {
    params$afterScrollHorizontally <- htmlwidgets::JS(
      "function() {
        var filter = document.getElementById(this.rootElement.id + '-filters');
        filter.scrollLeft = $(this.rootElement).find('.wtHolder')[0].scrollLeft;
      }"
    )
  }
  params
}
