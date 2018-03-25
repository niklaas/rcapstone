# Template taken from
#
#   https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
  required_aes = c("x"),
  default_aes = ggplot2::aes(y = .5,
                             colour = "black",
                             shape = 21,  # filled circle with border
                             fill = "black",
                             stroke = 1,
                             size = 1,
                             alpha = .5),
  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)

    # Grey lines that connect centroids of circles
    baselines <- purrr::map(unique(coords$y),
                            ~ grid::polylineGrob(x = c(min(coords$x), max(coords$x)),
                                                 y = rep(., times = 2),
                                                 id = rep(., times = 2),
                                                 gp = grid::gpar(
                                                   col = "black"
                                                 )))

    circles <- grid::pointsGrob(
                x = coords$x,
                y = coords$y,
                pch = coords$shape,
                size = grid::unit(scales::rescale(coords$size, to = c(0, 2)), "char"),
                gp = grid::gpar(
                  col = coords$colour,
                  fill = coords$fill,
                  alpha = coords$alpha
                )
              )

    grid::gTree(children = purrr::invoke(grid::gList, c(baselines, list(circles))))
  }
)

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}