install.packages("gganimate")
install.packages("viridis")
install.packages("tidyverse")
install.packages("wbstats")

library(tidyverse)
library(gganimate)
library(viridis)
library(wbstats)


wb_data(country = "all", indicator, startdate, enddate, mrv, return_wide = FALSE,
  gapfill, freq, cache, lang = c("en", "es", "fr", "ar", "zh"),
  removeNA = TRUE, POSIXct = FALSE, include_dec = FALSE,
  include_unit = FALSE, include_obsStatus = FALSE,
  include_lastUpdated = FALSE)

  wbcountries(lang = c("en", "es", "fr", "ar", "zh"))

  rosling_data <- wbstats::wb(indicator = c("SP.DYN.LE00.IN", "NY.GDP.PCAP.CD", "SP.POP.TOTL"), 
                       country = "countries_only", startdate = 1960, enddate = 2017) 

head(rosling_data %>% 
       dplyr::arrange(date, country), n = 3)

rosling_data <- rosling_data %>% 
  dplyr::left_join(wbstats::wbcountries() %>% 
                     dplyr::select(iso3c, region))

head(rosling_data %>% 
       dplyr::arrange(date, country), n = 3)


rosling_data <- rosling_data %>% 
  tidyr::pivot_wider(id_cols = c("date", "country", "region"), names_from = indicator, values_from = value)

head(rosling_data %>% 
       dplyr::arrange(date, country), n = 3)

rosling_data_2010 <- rosling_data %>% 
  dplyr::filter(date == 2010)


rosling_chart <- ggplot2::ggplot(rosling_data_2010, aes(x = `GDP per capita (current US$)`, 
                                                        y = `Life expectancy at birth, total (years)`, 
                                                        size = `Population, total`))

rosling_chart <- ggplot2::ggplot(rosling_data_2010, aes(x = `GDP per capita (current US$)`, 
                                                        y = `Life expectancy at birth, total (years)`, 
                                                        size = `Population, total`)) +
  ggplot2::geom_point(aes(color = region))


  rosling_chart