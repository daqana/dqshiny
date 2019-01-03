# Define server logic
shinyServer(
  function(input, output) {
    # initial data
    cars <- reactiveVal(datasets::mtcars)

    # render data to handsontable
    dq_render_handsontable(
      "cars", cars, filters = c("Auto", "Range", "Text"), reset = FALSE,
      page_size = 16L, sorting = TRUE, columns = -(1:2),
      table_param = list(rowHeaders = NULL)
    )

    # render data summary automatically in sync with user inputs
    output$table <- renderUI(dq_htmltable(
      as.data.frame.matrix(summary(cars())), align = "r", borders = "tex")
    )
  }
)
