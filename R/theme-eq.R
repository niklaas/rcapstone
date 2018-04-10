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