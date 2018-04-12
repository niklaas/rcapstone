#' Clean raw NOAA data
#' 
#' @param path The path to the raw \code{signif.txt} file as string
#' @return A tibble containing a cleaned data frame of the raw file
#' @export
#' @examples
#' \donttest{
#' noaa_data <- eq_clean_data("data-raw/signif.txt")
#' }
eq_clean_data <- function(path) {
  assert_that(
    is.string(path),
    fs::file_exists(path)
  )

  .data <- readr::read_tsv(file = path)

  assert_that(
    has_name(.data, "YEAR"),
    has_name(.data, "MONTH"),
    has_name(.data, "DAY")
  )

  # Unfortunately I don't have a clue how to parse negative dates. Commands such
  # as `lubridate::ymd("-1000-01-01")` return Date objects AD instead of BC.
  # `lubridate::parse_date_time(-1000, "Y")` returns an error. On the web I
  # found two resources mentioning BC dates:
  #
  #   * https://github.com/tidyverse/lubridate/issues/2
  #   * https://stackoverflow.com/a/14968817/2300759
  #
  # But both weren't helpful. The first is an abandoned GitHub issue and the
  # second doesn't work for me. So the following is a helper function that
  # subtracts the absolute value of a negative year of year 0 to achieve
  # negative dates.
  #
  # The function takes *vectors* as arguments.
  #
  # TODO: This works but is quite slow.
  parse_bc_dates <- function(years, months, days) {
    assert_that(
      is.numeric(years),
      is.numeric(months),
      is.numeric(days)
    )

    zero_year_dates <- lubridate::ymd(glue::glue("0000-{months}-{days}", .na = "01"))
    purrr::map2(zero_year_dates, years, function(zyd, y) {
      if (y < 0) zyd - lubridate::years(abs(y))
      else zyd + lubridate::years(y)
    }) %>% purrr::invoke(c, .)
  }

  .data <- .data %>%
    dplyr::mutate(
      DATE = parse_bc_dates(YEAR, MONTH, DAY),
      LOCATION = eq_location_clean(LOCATION_NAME)
    ) %>%
    dplyr::select(-YEAR, -MONTH, -DAY, -LOCATION_NAME)

  .data
}

globalVariables(c("YEAR", "MONTH", "DAY", "LOCATION_NAME"))