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
  df[min(l, ((p - 1) * size + 1)):min(l, p * size), , drop = FALSE]
}

#' @author richard.kunze
paging_row <- function(context, page_size, sizes) {
  shiny::fluidRow(
    class = "vertical-align paging-row",
    shiny::column(5, align = "right", "Page:"),
    shiny::column(1, shiny::numericInput(paste0("num_", context, "_page"), NULL, 1, 1)),
    shiny::column(2, shiny::textOutput(paste0(context, "_maxPages"), inline = TRUE)),
    shiny::column(3, align = "right", "Items per page:"),
    shiny::column(1, shiny::selectizeInput(paste0("sel_", context, "_pageSize"), NULL, sizes, page_size))
  )
}
