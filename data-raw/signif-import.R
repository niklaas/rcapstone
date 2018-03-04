devtools::load_all()

noaa_data <- eq_clean_data("data-raw/signif.txt")

devtools::use_data(noaa_data)