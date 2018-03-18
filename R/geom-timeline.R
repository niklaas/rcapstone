# Template taken from
#
#   https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(colour = "black", fill = "black", size = 1, alpha = .5),
  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)

    # TODO: Implement xmin and xmax

    # TODO: Plot grey line that connects centroids of circles

    grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      pch = 21,  # filled circle with border
      size = grid::unit(coords$size, "char"),
      gp = grid::gpar(
        col = coords$colour,
        fill = coords$fill,
        alpha = coords$alpha
      )
    )
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