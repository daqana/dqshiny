#' @title Adds a video box to the app
#'
#' @description video_box adds a box holding the given video file, which will be hidden
#' initially. Supports multiple sources.
#'
#' @param id id of the video element
#' @param src source(s) for the video,
#' @param title title to be shown above the video
#' @param type character specifying the video type(s)
#'
#' @export
#' @author richard.kunze
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' base_url <- "http://download.blender.org/peach/bigbuckbunny_movies/"
#' shinyApp(
#'   ui = fluidPage(
#'     video_box("lowRes",
#'       paste0(base_url, "BigBuckBunny_320x180.mp4"),
#'       "Low Resolution"
#'     ),
#'     video_box("highRes", type = "audio/ogg",
#'       paste0(base_url, "big_buck_bunny_720p_stereo.ogg")
#'     ),
#'     fluidRow(column(12,
#'       "Low resolution: ", video_tag("lowRes"), br(),
#'       "Higher resolution: ", video_tag("highRes", 300, "Bookmark at 5min")
#'     ))
#'   ),
#'   server = function(input, output) {}
#' )
#'
#' }
video_box <- function(id, src, title = NULL, type = "video/mp4") {
  if (length(type) != length(src)) {
    type <- rep_len(type, length(src))
  }
  shiny::div(
    id = paste0(id, "_wrapper"), class = "video-wrapper",
    shiny::div(
      style = "text-align:center;",
      shiny::h3(id = paste0(id, "_title"), class = "video-title", title),
      shiny::actionButton(
        paste0(id, "_hide"), NULL, shiny::icon("close"), style = "float:right;",
        onclick = paste0("$('#", id, "_wrapper').removeClass('shown');"),
        class = "dq-btn-sm"
      )
    ),
    shiny::tags$video(
      id = id, controls = "controls",
      lapply(seq(src), function(i) {
        shiny::tags$source(src = src[[i]], type = type[[i]])
      })
    ), init()
  )
}

#' @description video_tag adds a small button to show the corresponding
#' video box and jump directly to the given position in the video.
#'
#' @param time optional integer specifying the time to jump to (in seconds)
#' @export
#' @rdname video_box
video_tag <- function(id, time = NULL, title = NULL) {
  shiny::tags$div(
    class = "video-button", shiny::icon("play-circle"),
    onclick = paste0(
      "$('.video-wrapper').removeClass('shown');",
      "$('#", id, "_wrapper').addClass('shown');",
      if (!is.null(time)) paste0(
        "document.getElementById('", id, "').currentTime = ", time, ";"
      ),
      if (!is.null(title)) paste0(
        "$('#", id, "_title').text('", title, "');"
      )
    ), init()
  )
}
