#' Adds selectizeOptions to a column of rhandsontable
#'
#' @description dq_add_selectize_options adds selectizeOptions to a column of
#' a rhandsontable to be used with the selectize editor. Especially useful if
#' each cell should have individual dropdowns. It will also set the type and
#' editor for the specified column.
#'
#' @param hot rhandsontable object
#' @param rows vector of row indices, NULL means the whole column will be filled
#' @param col column index or name, must be scalar
#' @param options character vector or list to be used as selectize options,
#' names will be used as labels for the options, dq_add_selectize_options can
#' also handle lists of vectors, where each vector will be used to specify the
#' options of one cell
#' @param ... additional parameters to be passed to selectize
#'
#' @export
#' @return dq_add_selectize_options: updated rhandsontable object
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(rhandsontable)
#' library(shiny)
#' hw <- c("Hello", "my", "funny", "world!",
#'   "Those", "are", "some", "really", "random", "words!")
#'
#' options <- lapply(1:10, function(x) c(Name1 = sample(hw, 1),
#'   Name2 = sample(hw, 1), Name3 = sample(hw, 1)))
#' ch <- sample(hw, 3)
#' names(ch) <- sample(hw, 3)
#' selectize <- dq_as_selectize_options(ch, create = TRUE)
#'
#' empty <- rep("", 10)
#' df <- data.frame(Unlabled=empty, Labled=empty,
#'   Multiple=empty, stringsAsFactors = F)
#'
#' shinyApp(
#'   ui = fluidPage(
#'     dq_space(),
#'     rHandsontableOutput("randomTable")
#'   ),
#'   server = function(input, output) {
#'     output$randomTable <- renderRHandsontable({
#'       rhandsontable(df, stretchH = "all") %>%
#'         dq_add_selectize_options(NULL, 1, lapply(options, unname)) %>%
#'         hot_col(2, editor = "selectize", selectizeOptions = selectize) %>%
#'         dq_add_selectize_options(NULL, "Multiple", options, maxItems = 2)
#'     })
#'   }
#' )
#'
#' }
dq_add_selectize_options <- function(hot, rows, col, options, ...) {
  if (!inherits(hot, "rhandsontable")) return(hot)
  hot$dependencies <- append(hot$dependencies, selectize_dep)
  if (length(options) == 0) return(hot)
  if (is.null(rows)) {
    rows <- seq(hot$x$rowHeaders)
  }
  if (is.atomic(options)) {
    options <- lapply(rows, function(x) options)
  }
  if (length(rows) != length(options)) {
    options <- rep(options, length.out = length(rows))
  }
  if (is.character(col)) col <- which(hot$x$rColnames == col)
  if (length(col) == 1 && !is.na(col)) {
    lapply(rows, function(x) {
      if (length(options[[x]]) < 2 && all(options[[x]] == "")) return(NULL)
      hot <<- dq_hot_cell(
        hot, x, col, type = "dropdown", editor = "selectize",
        selectizeOptions = dq_as_selectize_options(options[[x]], ...)
      )
    })
  }
  hot
}

#' @description dq_as_selectize_options converts the given vector of options
#' into a proper selectize options list. Names of the given vector will be used
#' to specify labels for the options. Further selectize attributes can be set
#' via additional named parameters.
#'
#' @export
#' @return dq_as_selectize_options: list containing all options and additional
#' settings
#' @rdname dq_add_selectize_options
dq_as_selectize_options <- function(options, ...) {
  if (is.null(names(options))) names(options) <- options
  list(options = lapply(seq(options), function(y) list(
    value = unname(options[y]), text = names(options)[y]
  )), ...)
}


#' Configure individual cells of rhandsontable widget
#'
#' @description Configure individual cells of a rhandsontable widget. Can be
#' used just like \code{\link[rhandsontable:hot_cols]{hot_cols}} or
#' \code{\link[rhandsontable:hot_col]{hot_col}} to specify custom options or
#' cells. All possible combinations of row and column indices will be set.
#'
#' @param hot rhandsontable object
#' @param row integer vector specifying the rows to configure
#' @param col integer vector specifying the columns to configure
#' @param ... parameters to be set in the cells, can be all rhandsontable
#' parameters and additional custom ones used with custom renderers or editors
#'
#' @author richard.kunze
#' @export
#' @examples df <- data.frame(readOrWrite = rep(c("readOnly", "change me!"), 5),
#'   secret = rep("tops3cr3t", 10), stringsAsFactors = FALSE)
#'
#' hot <- rhandsontable::rhandsontable(df, rowHeaders = NULL)
#' hot <- dq_hot_cell(hot, seq(1, 10, 2), 1:2, readOnly = TRUE)
#' hot <- dq_hot_cell(hot, seq(1, 10, 2), 2, type = "password")
#' hot
dq_hot_cell <- function(hot, row, col, ...) {
  if (!inherits(hot, "rhandsontable")) return(hot)
  suppressWarnings({
    row <- as.integer(row)
    col <- as.integer(col)
  })
  row <- row[!is.na(row)]
  col <- col[!is.na(col)]
  if (length(row) == 0 || length(col) == 0) return(hot)
  lR <- length(row)
  lC <- length(col)
  rowInds <- rep(row, length.out = lR * lC)
  colInds <- rep(col, length.out = lR * lC, each = lR)
  cells <- lapply(seq(length(rowInds)), function(i) list(
    row = rowInds[i] - 1, col = colInds[i] - 1, ...
  ))
  hot$x$cell <- append(hot$x$cell, cells)
  hot
}
