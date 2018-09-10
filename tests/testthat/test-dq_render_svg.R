context("Test dq_render_svg in dq_render_svg.R")

test_that("dq_render_svg works and returns a closure for rendering", {
  plot <- dq_render_svg({ggplot2::ggplot(
    data = data.frame(x = seq(4), y = seq(4)),
    ggplot2::aes(x = x, y = y)) + ggplot2::geom_bar(stat = "identity")
  })
  expect_true(typeof(plot) == "closure")
})

