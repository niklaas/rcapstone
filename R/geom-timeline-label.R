#' Create labels for timeline
#'
#' @inheritParams ggplot2::geom_point
#' @section Aesthetics: `n_max` is the maximum number of labels that should
#'   be plotted as integer with length 1.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(noaa_data, aes(x = date, size = EQ_PRIMARY, n_max = 5)) +
#'   geom_timeline() +
#'   geom_timeline_label()
#' }
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_timeline_label
#' @keywords internal
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
  required_aes = c("x", "label"),
  default_aes = ggplot2::aes(y = .5,
                             colour = "black",
                             n_max = 3,
                             fontsize = 12,
                             size = 1),
  draw_key = ggplot2::draw_key_blank,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)

    coords <- dplyr::group_by(coords, y) %>%
      dplyr::top_n(coords$n_max[1], size)

    # For distance between centroids and text labels
    multiplier <- 1.2

    # Grey lines that connect centroids with text
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