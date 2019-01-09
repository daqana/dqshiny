#' @author richard.kunze
update_page <- function(df, page, size, session) {
  if (is.null(session) || is.null(df)) return()
  l <- nrow(df)
  if (length(page) != 1L || is.na(page)) page <- 1L
  else if (is.infinite(page)) page <- l
  if (length(size) != 1L || is.na(size)) size <- 25L
  else if (is.infinite(size)) size <- l

  maxP <- max(ceiling(l / size), 1L)
  p <- if (is.na(as.numeric(page))) 1L else page
  p <- max(min(p, maxP), 1L)
  shiny::updateNumericInput(
    session, "pageNum", value = p, max = maxP
  )
  df[min(l, ((p - 1L) * size + 1L)):min(l, p * size), , drop = FALSE]
}

#' @author richard.kunze
paging_row <- function(ns, page_size, sizes) {
  shiny::fluidRow(
    class = "vertical-align paging-row",
    shiny::column(5L, align = "right", "Page:"),
    shiny::column(1L, shiny::numericInput(ns("pageNum"), NULL, 1L, 1L)),
    shiny::column(2L, shiny::textOutput(ns("maxPages"), inline = TRUE)),
    shiny::column(3L, align = "right", "Items per page:"),
    shiny::column(1L, shiny::selectizeInput(ns("pageSize"), NULL, sizes, page_size))
  )
}
