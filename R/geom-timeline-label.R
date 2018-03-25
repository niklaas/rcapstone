# Template taken from
#
#   https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
  required_aes = c("x", "label", "size"),
  default_aes = ggplot2::aes(y = .5,
                             colour = "black",
                             n_max = 3,
                             fontsize = 12),
  draw_key = ggplot2::draw_key_blank,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)

    coords <- dplyr::arrange(coords, size)[1:coords$n_max[1], ]

    str(coords)

    # For distance between centroids and text labels
    multiplier <- 1.2

    # Grey lines that connect centroids with text
    # TODO: multiple Ys when there are multiple timelines on y axis
    lines <- grid::polylineGrob(x = rep(coords$x, times = 2),
                                y = c(coords$y, coords$y * multiplier),
                                id = rep(1:length(coords$y), times = 2),
                                gp = grid::gpar(col = "black"))

    text <- purrr::pmap(list(coords$label, coords$x, seq_along(coords$label)),
                        ~ grid::textGrob(label = ..1,
                                         x = ..2,
                                         y = coords$y[..3] * multiplier,
                                         just = c("left", "bottom"),
                                         rot = 45,
                                         gp = grid::gpar(
                                           fontsize = coords$fontsize[1]
                                         )))

    grid::gTree(children = purrr::invoke(grid::gList, c(list(lines), text)))
  }
)

geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}