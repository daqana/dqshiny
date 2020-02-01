# Define UI for application
library(shiny)
library(dqshiny)
library(rhandsontable)

shinyUI(
  fluidPage(
    tags$style("textarea {width:100%;height:500px;resize:none;font-family:monospace}"),
    column(2L, offset = 1L, dq_space(250), h4(paste(
      "First two columns of data are hidden but are still available in the",
      "reactiveVal so they can be used in the summary below:"
    ))),
    dq_handsontable_output("cars", width = 8L),
    dq_box(
      title = "Summary", width = 10L, offset = 1L, fill = FALSE,
      collapsed = TRUE, uiOutput("table")
    ),
    dq_box(
      title = "Code", width = 10L, offset = 1L, fill = FALSE,
      collapsed = TRUE, uiOutput("code")
    )
  )
)
