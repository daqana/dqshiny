library(shiny)
shinyApp(
  ui = fluidPage(
    dqshiny::dq_drawer(
      id = "left", direction = "left",
      A = "Content A", B = "Content B", C = "Content C"
    ),
    dqshiny::dq_drawer(
      id = "right", direction = "right",
      D = "Content D", E = "Content E", "F" = "Content F"
    ),
    dqshiny::dq_drawer(
      id = "top", direction = "top",
      G = "Content G", H = "Content H", I = "Content I"
    ),
    dqshiny::dq_drawer(
      id = "bottom", direction = "bottom",
      J = "Content J", K = "Content K", L = "Content L"
    )
  ),
  server = function(input, output) {}
)
