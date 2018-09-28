library(shiny)
shinyApp(
  ui = fluidPage(
    actionButton("btn_enable", "Enable"),
    actionButton("btn_disable", "Disable"),
    actionButton("btn_toggle", "Toggle State"),
    actionButton("btn_toggle_cond", "Toggle State with Condition"),
    actionButton("btn_toggle_all", "Toggle All States"),
    checkboxInput("condition", "Disabled"),
    dqshiny::dq_space(), # this is needed to make everything work
    actionButton("example1", "EXAMPLE1"),
    actionButton("example2", "EXAMPLE2"),
    actionButton("example3", "EXAMPLE3")
  ),
  server = function(input, output) {
    observeEvent(input$btn_enable, dqshiny::enable("example1"))
    observeEvent(input$btn_disable, dqshiny::disable("example1"))
    observeEvent(input$btn_toggle, dqshiny::toggle_state("example1"))
    observeEvent(input$btn_toggle_cond,
                 dqshiny::toggle_state("example1", input$condition)
    )
    observeEvent(input$btn_toggle_all,
                 dqshiny::toggle_state(c("example1", "example2", "example3"))
    )
  }
)
