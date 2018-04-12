#' Clean raw NOAA column LOCATION_NAME
#' 
#' The locations of earthquake erruptions stored in a \code{noaa_data} dataset
#' are separated by colons. This function extracts the third field containing
#' the name of the city. The function is only used internally for \code{eq_clean_data()}.
#'
#' @param col The column of an uncleaned \code{noaa_data} dataset that contains the locations
#'   of earthquake erruptions
#' @return A character vector containing a cleaned versioned of the locations
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

globalVariables(c("."))