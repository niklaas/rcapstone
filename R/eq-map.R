#' Create map with earthquakes
#'
#' @param x A data.frame that contains earthquake data by NOAA and was cleaned '
#'  with eq_clean_data()
#' @param annot_col  Unquoted name of column in \code{x} that holds text for pop-ups
#' @return A leaflet map created by leaflet::leaflet that indicates each
#'   earthquake with a circle. A pop-up displays the content of \code{annot_col}.
#' @export
#' @examples
#' eq_map(noaa_data, annot_col = date)
eq_map <- function(x, annot_col) {
  annot_col_quo <- enquo(annot_col)
  assert_that(
    inherits(x, "data.frame"),
    has_name(x, "LATITUDE"),
    has_name(x, "LONGITUDE"),
    has_name(x, quo_name(annot_col_quo))
  )

  leaflet::leaflet(data = x) %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
    leaflet::addCircleMarkers(
      lat = ~LATITUDE, lng = ~LONGITUDE,
      popup = annot_col_quo
    )
}