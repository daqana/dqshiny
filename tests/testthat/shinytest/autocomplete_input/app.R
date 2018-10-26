library(shiny)
library(dqshiny)
opts <- c("just", "a", "few", "words", "to", "check", "the", "functionality",
          "of", "this", "autocomplete", "input", "field")
shinyApp(
  ui = fluidPage(
    fluidRow(
      column(3,
        autocomplete_input("auto1", "Unnamed:", opts, max_options = 1000),
        autocomplete_input("auto2", "Named:", max_options = 1000,
          structure(opts, names = opts[order(opts)])),
        autocomplete_input("auto3", "Big data:", NULL, max_options = 1000,
          placeholder = "Big data taking several seconds to load ..."),
        actionButton("calc", "Calculate")
      ), column(3,
        tags$label("Value:"), verbatimTextOutput("val1", placeholder = TRUE),
        tags$label("Value:"), verbatimTextOutput("val2", placeholder = TRUE)
       )
    )
  ),
  server = function(input, output, session) {
    output$val1 <- renderText(as.character(input$auto1))
    output$val2 <- renderText(as.character(input$auto2))
    observeEvent(input$calc, {
      update_autocomplete_input(session, "auto3", placeholder = "Loaded!",
        options = rownames(mtcars))
    })
  }
)
