library(shiny)
shinyApp(
  ui = fluidPage(
    fluidRow(
      column(6, dqshiny::dq_gallery(
        id = "myGallery",
        tags$div(class = "content", plotOutput("plot1")),
        tags$div(class = "content", plotOutput("plot2")),
        tags$div(class = "content", plotOutput("plot3")),
        tags$div(class = "content", plotOutput("plot4"))
      )),
      column(6,
        actionButton("show3", "Show Plot 3"),
        actionButton("prev", "Previous Plot")
      )
    )
  ),
  server = function(input, output) {
    output$plot1 <- renderPlot(plot(mtcars$mpg, mtcars$cyl))
    output$plot2 <- renderPlot(plot(mtcars$disp, mtcars$hp))
    output$plot3 <- renderPlot(plot(mtcars$drat, mtcars$wt))
    output$plot4 <- renderPlot(plot(mtcars$qsec, mtcars$vs))

    observeEvent(input$show3, dqshiny::update_dq_gallery("myGallery", set = 3))
    observeEvent(input$prev, dqshiny::update_dq_gallery("myGallery", add = -1))
  }
)
