context("test-create-label.R")

test_data <- tibble::tribble(
  ~DATE,                        ~COUNTRY,         ~LOCATION,  ~EQ_PRIMARY,  ~TOTAL_DEATHS,
  lubridate::ymd("1858-04-23"), "Germany",        "Kiel",     1,            100,
  lubridate::ymd("1879-03-14"), "Germany",        "Ulm",      2,            101,
  lubridate::ymd("1942-01-08"), "England",        "Oxford",   3,            102,
  lubridate::ymd("1942-01-08"), "Eng<br />land",  "Oxford",   3,            102
)

test_that("single row", {
  expect_equal(
    eq_create_label(test_data[1, ]),
    as.character(glue::glue("
    <b>Location</b>: Kiel<br />
    <b>Magnitude</b>: 1<br />
    <b>Total deaths</b>: 100
    "))
  )
})

test_that("escapes HTML", {
  expect_equal(eq_create_label(test_data[3, ]), eq_create_label(test_data[4, ]))
})