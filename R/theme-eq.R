#' Minimal theme for timeline with earthquakes
#'
#' @param ... All options are piped to \code{ggplot2::theme_minimal()}
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(noaa_data, aes(x = date, size = EQ_PRIMARY, n_max = 5)) +
#'   geom_timeline() +
#'   theme_eq()
#' }
theme_eq <- function(...) {
  ggplot2::theme_minimal(...) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(colour = "black"),
      axis.ticks.x = ggplot2::element_line(colour = "black"),
      legend.position = "bottom",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
}