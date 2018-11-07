#' dqshiny: Enhance Shiny Apps with Customizable Modules
#'
#' @description Provides highly customizable modules to enhance your shiny apps.
#' Includes layout independent collapsible boxes and value boxes, a very fast
#' autocomplete input, rhandsontable extensions for filtering and paging and
#' much more.
#'
#' @section Section: some more info
#'   TO DO document the package!!
#'
#' @docType package
#' @name dqshiny
NULL

#' @author richard.kunze
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  init()
}

dq_dep <- htmltools::htmlDependency(
  "dqshiny", "0.0.1", c(href = "dqshinyRes"),
  script = "js/messageHandler.js",
  stylesheet = "css/dqshiny.css"
)

selectize_dep <- htmltools::tagList(
  htmltools::htmlDependency(
    "dqSelectize", "0.0.1", c(href = "dqshinyRes"),
    stylesheet = "css/handsontable-selectize-editor.css",
    script = "js/handsontable-selectize-editor.js"
  ), htmltools::htmlDependency(
    "selectize", "0.12.4", c(href = "shared/selectize"),
    stylesheet = "css/selectize.bootstrap3.css",
    head = format(htmltools::tagList(
      htmltools::HTML("<!--[if lt IE 9]>"),
      htmltools::tags$script(src = "shared/selectize/js/es5-shim.min.js"),
      htmltools::HTML("<![endif]-->"),
      htmltools::tags$script(src = "shared/selectize/js/selectize.min.js")
    ))
  )
)

fontawesome_dep <- htmltools::tagList(
  htmltools::htmlDependency(
    "font-awesome", "4.7.0", c(href = "shared/font-awesome/css"),
    stylesheet = "font-awesome.min.css"
  ),
  htmltools::htmlDependency(
    "font-awesome", "5.3.1", c(href = "shared/fontawesome/css"),
    stylesheet = "all.min.css"
  )
)

jqueryui_dep <- htmltools::htmlDependency(
  "jqueryui", "1.12.1", c(href = "shared/jqueryui"),
  script = "jquery-ui.min.js"
)

not_null <- function(vec) {
  vec[!vapply(vec, is.null, TRUE)]
}
