#' Returns daqana header in pure HTML code
#'
#' @description Creates the daqana header in dependency of the given log and title.
#' If no customer logo is given, just daqana logo will be in header.
#' @param logo src of the daqana logo, or text
#' @param link hyperlink
#' @param fullscreen_menu add fullscreen menu to site
#' @family html creating functions
#'
#' @return string representing the html code
#' @export
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     dq_header()
#'   ),
#'   server = function(input, output) {
#'   }
#' )}
dq_header <- function(logo = "dqshinyRes/img/logo_daqana.svg", link = "https://www.daqana.com", fullscreen_menu = TRUE) {
  res <- paste0("<header id='main-header'",
                "<div class='container clearfix menu_container'>",
                "<div class='logo_container'><span class='logo_helper'></span>",
                "<a href='", link, "' target='_blank'>",
                if (any(endsWith(tolower(logo), c(".jpg", ".jpeg", ".svg", ".png", ".gif"))))
                  paste0("<img alt='Daqana Analytics' class='logo' id='logo' src='", logo, "'>") else
                    paste0("<h1 id='logo'>", logo, "</h1>"),
                "</a></div>",
                if (fullscreen_menu) {
                  paste0(
                    "<div id='top-navigation'><span class='fa_mobile_menu_bar header_toggle toggle_fullscreen_menu'>",
                    shiny::icon("bars"), "</span></div>")
                } else "",
                "</div></header>")
  return(shiny::HTML(res))
}

#' Returns daqana headline
#'
#' @description Creates a daqana headline
#' @param h1 title, large text
#' @param h4 description, small text
#' @family html creating functions
#'
#' @return shiny fluid row containing daqana headline
#' @export
#' @author david.breuer
#' @examples \donttest{library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     dq_header(),
#'     dq_headline()
#'   ),
#'   server = function(input, output) {
#'   }
#' )}
dq_headline <- function(h1 = "", h4 = "") {
  shiny::fluidRow(class = "orange headline",
                  shiny::column(12, shiny::HTML(paste0("<h1>", h1, "&nbsp</h1><h4>", h4, "</h4>")))
  )
}

#' Returns daqana fullscreen menu in pure HTML code
#'
#' @param items list of items to show in the menu
#'
#' @description Creates the daqana fullscreen menu as html code.
#' @family html creating functions
#'
#' @return string representing the html code
#' @export
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     dq_fullscreen_menu(),
#'     dq_header()
#'   ),
#'   server = function(input, output) {
#'   }
#' )}
dq_fullscreen_menu <- function(items) {
  res <- paste0(
    "<div class='slide_in_menu_container'>
	    <span class='fa_mobile_menu_bar toggle_fullscreen_menu'></span>
        <div class='slide_menu_top'>
          <div class='top_menu_inner'></div>
        </div>
    </div>
    <div class='fullscreen_nav_container'>
      <ul id='mobile_menu_slide' class='mobile_menu'>"
  )
  for (item in items) {
    res <- menu_entry(item, TRUE, res)
  }
  res <- paste0(res, "</ul></div></div>")
  return(shiny::HTML(res))
}

#' Creates a custom footer
#'
#' @description Creates a custom HTML footer with dq orange background and the given HTML code inside.
#'
#' @param html HTML code to show within the footer
#' @param always_botton specify if this footer should always be on the bottom of the window, even if the body isn't as high as the window
#'
#' @return fluidRow with the custom footer
#' @export
#' @author richard.kunze
#' @examples \donttest{library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     init(),
#'     dq_fullscreen_menu(),
#'     dq_header(),
#'     dq_footer()
#'   ),
#'   server = function(input, output) {
#'   }
#' )}
dq_footer <- function(html = NULL, always_botton = TRUE) {
  if (is.null(html)) {
    html <- paste0("Copyright ", substr(Sys.Date(), 1, 4), ", <a href='https://www.daqana.com/'>daqana GmbH</a>, All rights reserved.")
  }
  shiny::fluidRow(id = "div_footer",
           class = paste0("orange", ifelse(always_botton, " alwaysOnBotton", "")),
           shiny::column(12, class = "help-block", shiny::HTML(html)))
}

menu_entry <- function(item, fullscreenMenu = FALSE, str = "") {
  hasChilds <- !is.null(item$children)
  str <- paste0(str, "<li class='menu-item",
                if (hasChilds) " menu-item-has-children" else "",
                "'><a href='", item$link, "'>",
                item$name, if (hasChilds) "" else "</a></li>")
  if (hasChilds) {
    str <- paste0(str, "<span class='mobile_menu_arrow'>", shiny::icon("angle-up"),
                  "</span></a><ul class='sub-menu' style='display: none;'>")
    for (c in item$children) {
      str <- menu_entry(c, fullscreenMenu, str)
    }
    str <- paste0(str, "</li></ul>")
  }
  return(str)
}
