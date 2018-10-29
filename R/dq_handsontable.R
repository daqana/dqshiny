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
  shiny::fluidRow(shiny::column(
    width, offset = offset,
    shiny::uiOutput(paste0(id, "_filters")),
    rhandsontable::rHandsontableOutput(id),
    shiny::uiOutput(paste0(id, "_pages")),
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
#' also be reactive or a reactiveValues object holding the data under the given
#' id (e.g. myReactiveValues[[id]] <- data). In case of reactiveValues, those
#' will always be in line with user inputs.
#' @param context the context used to specify all ui elements used for this
#' table, can be omitted which ends up in a randomly generated context
#' @param filters optional character vector, adds filters for each column,
#' values must be one of "Text", "Select", "Range", "Date", "Auto" or "" (can be
#' abbreviated) to add a Text-, Select-, Range-, DateRange-, AutocompleteInput
#' or none, vectors of length one will add a filter of this type for each column
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
#' @param horizontal_scroll optional boolean to scroll the filterrow according
#' to the hot table, especially useful for tables with many columns
#' @param table_param optional list, specify parameters to hand to rhandsontable
#' table element
#' @param cols_param optional list, specify parameters to hand to rhandsontable
#' cols elements
#' @param col_param optional list of lists to specify parameters to hand to
#' rhandsontable col elements
#' @param cell_param optional list of lists to specify parameters to hand to
#' rhandsontable cells
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
#'       filters = c("S", "T", "R", "D"), sorting = c(dir = "up", col = "B"),
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
  table_param = NULL, cols_param = NULL, col_param = NULL, cell_param = NULL
) {
  requireNamespace("rhandsontable", quietly = TRUE)
  requireNamespace("shiny", quietly = TRUE)

  # initial settings
  if (is.null(id) || is.null(data)) return()
  if (is.null(context)) context <- paste0(sample(letters, 6L), collapse = "")
  if (length(columns) == 0L) columns <- TRUE

  session <- shiny::getDefaultReactiveDomain()
  input <- session$input
  output <- session$output

  paged <- length(page_size) > 0L && page_size > 0L
  page_id <- context

  dq_values <- shiny::reactiveValues()

  page_select <- paste0("sel_", context, "_pageSize")
  page_num <- paste0("num_", context, "_page")
  to_sort <- (length(sorting) > 0L && sorting != FALSE)

  # used functions
  update_page_if_necessary <- function() {
    if (paged) {
      sel <- as.integer(input[[page_select]])
      dq_values[[page_id]] <- update_page(
        dq_values[[context]], context, input[[page_num]], sel, session
      )
    }
  }

  set_data <- function(df) {
    df <- as.data.frame(df)
    dq_values$full <- dq_values[[context]] <- df
    if (length(df) > 0L) dq_values[[context]] <- df[, columns, drop = FALSE]
  }

  if (shiny::is.reactivevalues(data)) {
    shiny::observeEvent(data[[id]], {
      set_data(data[[id]])
      update_page_if_necessary()
    }, ignoreInit = TRUE)
    set_data(shiny::isolate(data[[id]]))
  } else if (shiny::is.reactive(data)) {
    shiny::observe({
      set_data(data())
      update_page_if_necessary()
    })
    set_data(shiny::isolate(data()))
  } else {
    set_data(data)
  }

  # define page_id which is needed for table rendering and reduce data to first page
  if (paged) {
    page_id <- paste0(context, "Page")
    df <- shiny::isolate(dq_values[[context]])
    n <- min(page_size[1L], nrow(df))
    dq_values[[page_id]] <- df[1:n,]
  }

  # render filter row and add observer for filters
  if (!all(filters == "") && !is.null(output)) {
    output[[paste0(id, "_filters")]] <- shiny::renderUI({
      filter_row(context, dq_values, filters, reset, sorting)
    })
    shiny::observeEvent(get_filters(input, context), {
      f_vals <- get_filters(input, context)
      if (length(f_vals) == 0) return()
      df <- text_filter(dq_values$full[, columns, drop = FALSE], f_vals[sapply(f_vals, function(x) length(x) == 1)])
      dq_values[[context]] <- range_filter(df, f_vals[sapply(f_vals, function(x) length(x) > 1)])
      if (to_sort) {
        dq_values[[context]] <- sort_data(
          dq_values[[context]], dq_values$sort_dir, dq_values$sort_col
        )
      }
      update_page_if_necessary()
    }, ignoreInit = TRUE)
  }

  # merge default table/cols parameters with given ones
  table_default <- list(readOnly = FALSE, stretchH = "all", contextMenu = FALSE)
  table_default <- append(table_param, table_default)
  table_default <- table_default[!duplicated(names(table_default))]

  cols_default <- list(colWidths = 1L, highlightCol = TRUE,
                       highlightRow = TRUE, manualColumnResize = TRUE)
  cols_default <- append(cols_param, cols_default)
  cols_default <- cols_default[!duplicated(names(cols_default))]

  params <- list(table_default, cols_default, col_param, cell_param)
  params[[1L]] <- add_scripts(params[[1L]], isTRUE(width_align),
                              isTRUE(horizontal_scroll))

  # render dq_handsontable
  if (!is.null(output)) {
    output[[id]] <- rhandsontable::renderRHandsontable({
      params[[1L]]$data <- dq_values[[page_id]]
      params[[2L]]$hot <- do.call(rhandsontable::rhandsontable, params[[1L]])
      hot <- do.call(rhandsontable::hot_cols, params[[2L]])
      for (x in params[[3L]]) {
        hot <- do.call(rhandsontable::hot_col, append(list(hot), x))
      }
      for (x in params[[4L]]) {
        x$row <- match(x$row, rownames(dq_values[[page_id]]))
        x$row <- x$row[!is.na(x$row)]
        hot <- do.call(dq_hot_cell, append(list(hot), x))
      }
      hot$dependencies <- append(hot$dependencies, init())
      hot
    })
  }

  # render paging row and add observer for inputs
  if (paged && !is.null(output)) {
    page_sizes <- sort(unique(c(page_size, 10L, 25L, 50L, 100L)))
    output[[paste0(id, "_pages")]] <- shiny::renderUI({
      paging_row(context, page_size[1L], page_sizes)
    })
    output[[paste0(context, "_maxPages")]] <- shiny::renderText({
      sel <- as.integer(input[[page_select]])
      paste("of ", ceiling(max(length(dq_values[[context]][[1L]]) / sel, 1L)))
    })
    shiny::observeEvent(c(input[[page_num]], input[[page_select]]), {
      if (!is.na(input[[page_select]])) update_page_if_necessary()
    })
  }

  # add sort buttons
  if (to_sort) {
    sorts <- add_sorting_observer(input, session, dq_values, context, paged, page_id)
  }

  # add reset button
  if (reset) {
    shiny::observeEvent(input[[paste0("reset_", context, "_filter")]], {
      for (n in grep(paste0("^filter_", context), names(input), value = TRUE)) {
        shiny::updateTextInput(session, n, value = "")
        reset_slider_input(n)
      }
      if (to_sort) {
        dq_values$sort_dir <- dq_values$sort_col <- ""
        lapply(sorts, function(n) update_icon_state_button(session, n, value = 1L))
      }
    })
  }

  # add observer for table changes
  shiny::observeEvent(input[[id]], {
    if (!is.null(input[[id]]$changes$source)) {
      row_names <- as.character(rownames(rhandsontable::hot_to_r(input[[id]])))
      col_names <- colnames(dq_values[[context]])
      lapply(input[[id]]$changes$changes, function(ch) {
        row <- ch[[1L]] + 1L
        col <- ch[[2L]] + 1L
        dq_values[[context]][row_names[row], col] <- ch[[4L]]
        dq_values$full[row_names[row], col_names[col]] <- ch[[4L]]
      })
      if (shiny::is.reactivevalues(data)) {
        data[[id]] <- dq_values$full
      }
      if (!is.null(filters)) {
        update_filters(dq_values$full, filters, context, session)
      }
    }
  }, ignoreInit = TRUE)

  shiny::isolate(dq_values$full)
}

#' @author richard.kunze
add_scripts <- function(params, width, scroll) {
  if (width || scroll) {
    params$afterRender <- htmlwidgets::JS(paste0(
      "function() {
        var hider = $(this.rootElement).find('.wtHider');
        var $filter = $(document.getElementById(this.rootElement.id + '_filters'));
        var that = this;
        $filter.ready(function() {
          $filter.css('overflow', 'hidden');
          var row = $filter.find('.row');
          row.width(hider.width());",
      if (width)
        "var els = $filter.find('.form-group'), sum = 0, w, cW;
         if (that.params) {
           cW = that.params.colWidths;
           const reset = $filter.find('.reset-wrapper');
           w = hider.width() - (reset.length ? reset.width() : 0);
           if (cW) {
             if (cW.length) for (var i = 0; i < cW.length; i++) sum += cW[i];
             else sum = els.length * cW;
           }
         }
         for (var i = 0; i < els.length; i++) {
           els[i].style.width = (sum ? w / sum * that.getColWidth(i) : that.getColWidth(i)) + 'px';
         }
       });",
      "}"))
  }
  if (scroll) {
    params$afterScrollHorizontally <- htmlwidgets::JS(
      "function() {
        var filter = document.getElementById(this.rootElement.id + '_filters');
        filter.scrollLeft = $(this.rootElement).find('.wtHolder')[0].scrollLeft;
      }")
  }
  params
}
