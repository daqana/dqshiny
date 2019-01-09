library(shiny)
shinyApp(
  ui = fluidPage(
    dqshiny::dq_handsontable_output("randomTable", 9L)
  ),
  server = function(input, output, session) {
    hw <- c("hello", "my", "funny", "world!")
    data <- data.frame(A = rep(hw, 500), B = hw[c(2L, 3L, 4L, 1L)],
      C = 1:500, D = 501:1000, stringsAsFactors = FALSE)

    dqshiny::dq_render_handsontable("randomTable", data,
      filters = c("S", "T", "R", "A"), sorting = TRUE,
      page_size = c(17L, 5L, 500L, 1000L),
      col_param = list(list(col = 1L, type = "dropdown", source = letters)),
      cell_param = list(list(row = 2:9, col = 2L, readOnly = TRUE))
    )
  }
)
