#' Clean raw NOAA column LOCATION_NAME
#'
#' Given the LOCATION_NAME column from a raw NOAA database, this function
#' removes the name of the country at the beginning of each entry.
eq_location_clean <- function(col) {
  assert_that(
    is.character(col)
  )
  stringr::str_split(col, ":") %>%
    purrr::map_chr(~ .[length(.)]) %>%
    stringr::str_replace("^\\s+", "") %>%
    stringr::str_replace("\\s+$", "") %>%
    stringr::str_to_title()
}