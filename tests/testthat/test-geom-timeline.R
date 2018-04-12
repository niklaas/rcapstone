context("test-geom-timeline.R")

test_data <- tibble::tribble(
  ~DATE,                        ~COUNTRY,   ~LOCATION,  ~EQ_PRIMARY,
  lubridate::ymd("1858-04-23"), "Germany",  "Kiel",     1,
  lubridate::ymd("1879-03-14"), "Germany",  "Ulm",      2,
  lubridate::ymd("1942-01-08"), "England",  "Oxford",   3
)

test_that("geom_timeline", {
  vdiffr::expect_doppelganger("x aesthetic only",
    ggplot2::ggplot(test_data, ggplot2::aes(x = DATE)) + geom_timeline()
  )
})
