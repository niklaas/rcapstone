devtools::load_all()

erruptions <- noaa_data

ggplot2::ggplot(erruptions[1:10, ],
                mapping = ggplot2::aes(x = date,
                                       y = COUNTRY)) +
  geom_timeline()