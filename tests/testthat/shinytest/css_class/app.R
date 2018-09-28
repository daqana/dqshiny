library(shiny)
shinyApp(
  ui = fluidPage(
    tags$head(tags$style(".orange{background:#ff8f00}")),
    actionButton("add", "Add Class"),
    actionButton("remove", "Remove Class"),
    actionButton("toggle", "Toggle Class"),
    actionButton("toggle_cond", "Toggle Class with Condition"),
    checkboxInput("condition", "orange"),
    fluidRow(id = "row",
             dqshiny::dq_space(), # this is needed to make everything work
             actionButton("example", "EXAMPLE"),
             dqshiny::dq_space() # this is just for the alignment ;)
    )
  ),
  server = function(input, output) {
    observeEvent(input$add, dqshiny::add_class("row", "orange"))
    observeEvent(input$remove, dqshiny::remove_class("row", "orange"))
    observeEvent(input$toggle, dqshiny::toggle_class("row", "orange"))
    observeEvent(
      input$toggle_cond,
      dqshiny::toggle_class("row", "orange", input$condition)
    )
  }
)
