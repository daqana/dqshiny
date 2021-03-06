% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/video_box.R
\name{video_box}
\alias{video_box}
\alias{video_tag}
\title{Adds a video box to the app}
\usage{
video_box(id, src, title = NULL, type = "video/mp4")

video_tag(id, time = NULL, title = NULL)
}
\arguments{
\item{id}{id of the video element}

\item{src}{source(s) for the video,}

\item{title}{title to be shown above the video}

\item{type}{character specifying the video type(s)}

\item{time}{optional integer specifying the time to jump to (in seconds)}
}
\description{
video_box adds a box holding the given video file, which will be hidden
initially. Supports multiple sources.

video_tag adds a small button to show the corresponding
video box and jump directly to the given position in the video.
}
\examples{
## Only run examples in interactive R sessions
if (interactive()) {

library(shiny)
base_url <- "http://download.blender.org/peach/bigbuckbunny_movies/"
shinyApp(
  ui = fluidPage(
    video_box("lowRes",
      paste0(base_url, "BigBuckBunny_320x180.mp4"),
      "Low Resolution"
    ),
    video_box("highRes", type = "audio/ogg",
      paste0(base_url, "big_buck_bunny_720p_stereo.ogg")
    ),
    fluidRow(column(12,
      "Low resolution: ", video_tag("lowRes"), br(),
      "Higher resolution: ", video_tag("highRes", 300, "Bookmark at 5min")
    ))
  ),
  server = function(input, output) {}
)

}
}
\author{
richard.kunze
}
