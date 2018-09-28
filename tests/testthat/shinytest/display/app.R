library(shiny)
shinyApp(
  ui = fluidPage(
    dqshiny::init(),
    actionButton("hide", "Hide"),
    actionButton("show", "Show"),
    actionButton("toggle", "Toggle"),
    actionButton("toggle_cond", "Toggle Visibility with Condition"),
    actionButton("toggle_all", "Toggle All Visibilities"),
    checkboxInput("condition", "Visible"),
    actionButton("example1", "EXAMPLE1"),
    actionButton("example2", "EXAMPLE2"),
    actionButton("example3", "EXAMPLE3")
  ),
  server = function(input, output) {
    observeEvent(input$hide, dqshiny::hide("example1"))
    observeEvent(input$show, dqshiny::show("example1"))
    observeEvent(input$toggle, dqshiny::toggle("example1"))
    observeEvent(input$toggle_cond,
                 dqshiny::toggle("example1", input$condition)
    )
    observeEvent(input$toggle_all,
                 dqshiny::toggle(c("example1", "example2", "example3"))
    )
  }
)
