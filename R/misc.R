#' @author richard.kunze
#' @keywords internal
.onAttach <- function(...) {
  shiny::addResourcePath(
    "dqshinyRes", system.file("www", package = "dqshiny")
  )
}

dq_dep <- htmltools::htmlDependency(
  "dqshiny", "0.0.1", c(href = "dqshinyRes"),
  script = "js/messageHandler.js",
  stylesheet = "css/dqShiny.css"
)

selectize_dep <- htmltools::tagList(
  htmltools::htmlDependency(
    "dqSelectize", "0.0.1", c(href = "dqshinyRes"),
    stylesheet = "css/handsontable-selectize-editor.css",
    script = "js/handsontable-selectize-editor.js"
  ), htmltools::htmlDependency(
    "selectize", "0.11.2", c(href = "shared/selectize"),
    stylesheet = "css/selectize.bootstrap3.css",
    head = format(htmltools::tagList(
      htmltools::HTML("<!--[if lt IE 9]>"),
      htmltools::tags$script(src = "shared/selectize/js/es5-shim.min.js"),
      htmltools::HTML("<![endif]-->"),
      htmltools::tags$script(src = "shared/selectize/js/selectize.min.js")
    ))
  )
)

fontawesome_dep <- htmltools::htmlDependency(
  "font-awesome", "4.7.0", c(href = "shared/font-awesome"),
  stylesheet = "css/font-awesome.min.css"
)

jqueryui_dep <- htmltools::htmlDependency(
  "jqueryui", "1.12.1", c(href = "shared/jqueryui"),
  script = "jquery-ui.min.js"
)
