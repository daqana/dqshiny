# Define server logic
library(shiny)
library(dqshiny)
library(rhandsontable)

shinyServer(
  function(input, output, session) {
    # initial data
    cars <- reactiveVal(datasets::mtcars)

    # render data to handsontable
    dq_render_handsontable(
      "cars", cars, reset = FALSE, filters = c(NA, "Auto"),
      page_size = 16L, sorting = c(dir = "up", col = "hp"), columns = -(1:2),
      table_param = list(rowHeaders = NULL)
    )

    # render data summary automatically in sync with user inputs
    output$table <- renderUI(dq_htmltable(
      as.data.frame.matrix(summary(cars())), align = "r", borders = "tex")
    )
  }
)
