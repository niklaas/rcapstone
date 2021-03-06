#' Create timeline of earthquakes
#'
#' @inheritParams ggplot2::geom_point
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(noaa_data, aes(x = DATE, size = EQ_PRIMARY, n_max = 5)) +
#'   geom_timeline()
#' }
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_timeline
#' @keywords internal
#' @export
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

    # NOTE: I don't implement neither xmin nor xmax here. I think this should be
    # done by providing an appropriate data.frame to geom_timeline() that
    # filters in the first place.

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
                gp = grid::gpar(
                  alpha = coords$alpha,
                  col = coords$colour,
                  fill = coords$fill,
                  fontsize = coords$size * ggplot2::.pt + coords$stroke * ggplot2::.stroke /2
                )
              )

    grid::gTree(children = purrr::invoke(grid::gList, c(baselines, list(circles))))
  }
)
