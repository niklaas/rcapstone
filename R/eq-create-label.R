#' Create pop-up label for map
#'
#' @param x A data.frame that contains earthquake data by NOAA and was cleaned
#'   with eq_clean_data()
#' @return A character vector holding the contents of a pop-up for each
#'   earthquake formatted as HTML
#' @export
#' @examples
#' labels <- eq_create_label(noaa_data)
eq_create_label <- function(x) {
  assert_that(
    has_name(x, "LOCATION"),
    has_name(x, "EQ_PRIMARY"),
    has_name(x, "TOTAL_DEATHS")
  )

  markup_line <- function(key, value) {
    assert_that(
      is.string(key),
      length(value) == 1
    )
    if (is.na(value)) NA
    else paste0("<b>", key, "</b>: ", value)
  }

  markup_popup <- function(lines) {
    assert_that(
      is.character(lines)
    )
    paste0(lines[!is.na(lines)], collapse = "<br />\n")
  }

  purrr::pmap_chr(x,
    function(LOCATION, EQ_PRIMARY, TOTAL_DEATHS, ...) {
      markup_popup(c(
        markup_line("Location", htmltools::htmlEscape(LOCATION)),
        markup_line("Magnitude", htmltools::htmlEscape(EQ_PRIMARY)),
        markup_line("Total deaths", htmltools::htmlEscape(TOTAL_DEATHS))
      ))
    }
  )
}