
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://api.travis-ci.org/daqana/dqshiny.svg?branch=master)](https://travis-ci.org/daqana/dqshiny)
[![Codecov test
coverage](https://codecov.io/gh/daqana/dqshiny/branch/master/graph/badge.svg)](https://codecov.io/gh/daqana/dqshiny?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/dqshiny)](https://cran.r-project.org/package=dqshiny)
[![Downloads](http://cranlogs.r-pkg.org/badges/dqshiny?color=brightgreen)](http://www.r-pkg.org/pkg/dqshiny)

# dqshiny

The goal of dqshiny is to provide highly customizable modules to enhance
your shiny apps. Includes layout independent collapsable boxes and value
boxes, a very fast autocomplete input, rhandsontable extensions for
filtering and paging and much more.

## Installation

`dqshiny` is on CRAN so it can be installed with:

``` r
install.packages("dqshiny")
```

You can install the latest development version of `dqshiny` using:

``` r
remotes::install_github("daqana/dqshiny")
```

## Example

There’s a package demo `dqshiny-base-features` demonstrating some
functionalities. Or you can start with this short example to see some
simple boxes in action:

``` r
library(shiny)
library(dqshiny)
shinyApp(
  ui = fluidPage(
    fluidRow(
      dq_box(
        title = "Say Hello to my children", collapsed = TRUE, fill = FALSE,
        dq_infobox("Hello", 2, "World", icon("hashtag"), bg_color = "black", color = "#D00"),
        dq_box(title = "Box in the box", bg_color = "red", width = 8, dq_space())
      )
    )
  ),
  server = function(input, output) {}
)
```

If you have a bigger amount of selectable choices and still don’t want
to use textInputs you may have a look at this:

``` r
library(shiny)
library(dqshiny)
# create 100k random words
opts <- sapply(1:100000, function(i) paste0(sample(letters, 7), collapse=""))
shinyApp(
  ui = fluidPage(
    fluidRow(
      column(3,
        autocomplete_input("auto1", "Unnamed:", opts, max_options = 1000),
        autocomplete_input("auto2", "Named:", max_options = 1000,
          structure(opts, names = opts[order(opts)]))
        # feel free to test this with select... and may get yourself a coffee
        # , selectInput("sel", "Select:", opts)
      ), column(3,
        tags$label("Value:"), verbatimTextOutput("val1", placeholder = TRUE),
        tags$label("Value:"), verbatimTextOutput("val2", placeholder = TRUE)
      )
    )
  ),
  server = function(input, output) {
    output$val1 <- renderText(as.character(input$auto1))
    output$val2 <- renderText(as.character(input$auto2))
  }
)
```

If you ever encountered the wish to have a paged or filtered
rHandsontable, this example could be quite interesting:

``` r
library(shiny)
library(dqshiny)
shinyApp(
  ui = fluidPage(dq_handsontable_output("myTable", 9L)),
  server = function(input, output, session) {
    hw <- c("Hello", "my", "funny", "world!")
    data <- data.frame(A = rep(hw, 500L), B = hw[c(2:4, 1L)], C = 1:500, D = 500:1)
    dq_render_handsontable("myTable", data,
      filters = c("Sel", "Text", NA, "Auto"), sorting = TRUE,
      page_size = c(17L, 5L, 500L, 1000L),
      col_param = list(list(col = 3L, format = "0.00")),
      cell_param = list(list(row = 2:9, col = 2L, readOnly = TRUE))
    )
  }
)
```
